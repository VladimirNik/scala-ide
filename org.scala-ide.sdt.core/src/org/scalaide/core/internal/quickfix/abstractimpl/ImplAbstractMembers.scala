package org.scalaide.core.internal.quickfix.abstractimpl

import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal
import org.scalaide.core.internal.jdt.model.ScalaSourceFile
import scala.collection.immutable
import org.scalaide.core.internal.quickfix.explicit.ExpandText
import scala.tools.refactoring.implementations.AddToClosest
import org.eclipse.jface.text.Position
import org.eclipse.jdt.core.ICompilationUnit
import org.scalaide.core.internal.quickfix.createmethod.{ ParameterList, ReturnType }
import scala.tools.refactoring.implementations.AddMethod
import scala.tools.refactoring.implementations.AddMethodTarget
import org.scalaide.util.internal.scalariform.ScalariformParser
import org.eclipse.jface.text.contentassist.IContextInformation
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.graphics.Point
import org.eclipse.text.edits.ReplaceEdit
import org.eclipse.jface.text.IDocument
import org.eclipse.jdt.internal.ui.JavaPluginImages
import org.scalaide.core.internal.jdt.model.ScalaCompilationUnit
import org.scalaide.util.internal.eclipse.EditorUtils
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.util.NoPosition

object ImplAbstractMembers {
  def suggestsFor(ssf: ScalaSourceFile, offset: Int): Array[IJavaCompletionProposal] = {
    implAbstractMember(ssf, offset).toArray
  }

  def implAbstractMember(ssf: ScalaSourceFile, offset: Int): List[IJavaCompletionProposal] = {
    ssf.withSourceFile { (srcFile, compiler) =>
      import compiler._ //{ Tree, ImplDef, ClassDef, ModuleDef, EmptyTree, TypeTree, Type, Symbol, Name }

      type NamedTree = ImplDef//Tree { val name: Name; val impl: Tree }
      type TParameterList = List[String]

      class AbstractMemberProposal(abstrMember: Symbol, cl: NamedTree, target: AddMethodTarget) extends IJavaCompletionProposal {
        private def initValOrDef: (TParameterList, ParameterList, ReturnType, Boolean) = {
          //TODO find last printed method
          //TODO in scala-refactoring we can check with pos.isDefined
          def refactContextPos =
            if (!Option(cl.impl).isEmpty && !cl.impl.isEmpty && !cl.impl.children.isEmpty && cl.impl.children.last.pos.isDefined && cl.impl.children.last.pos.isRange)
              createPosition(srcFile, cl.impl.children.last.pos.end)
            else createPosition(srcFile, cl.pos.end)

          //TODO change rangePos and move it to common for all AbstractMemberProposal (it should be one for all)
          val rangePos = refactContextPos
          val sprinterContext = compiler.locateContext(rangePos).getOrElse(compiler.doLocateContext(createPosition(srcFile, offset)))

          def processType(tp: Type) =
            (if (tp.isError) None
            else if (tp.toString == "Null") Some("AnyRef") //there must be a better condition
            else {
              //TODO remove sprinter
              //TODO change to pass Option[Context]
              //val sprinterType = TypePrinters.showType(compiler, tp, sprinterContext)
              //Some(sprinterType) //do we want tpe.isError? tpe.isErroneous?
              Some(tp.toString())
            }) getOrElse ("Any")

          val method = abstrMember.asMethod
          val paramss: ParameterList = method.paramss map {
            _.zipWithIndex.map { param =>
              ((if (param._1.isImplicit && (param._2 == 0)) "implicit " else "") + param._1.name.decode, processType(param._1.tpe.asSeenFrom(cl.symbol.tpe, method.owner)))
            }
          }

          //val paramss: ParameterList = List(List(("x", "Float")))
          //val retType = Option("Double")

          val tparams: TParameterList = method.typeParams map (_.name.decode)
          val retType: ReturnType = Option(processType(method.returnType.asSeenFrom(cl.symbol.tpe, method.owner)))
          //TODO fix isDef
//          val isDef = abstrMember.isMethod && !(abstrMember.isVal || abstrMember.isVar || abstrMember.isMutable || abstrMember.isVariable)
          val isDef = abstrMember.isMethod && !abstrMember.isAccessor
          (tparams, paramss, retType, isDef)
        }

        private val (typeParameters: TParameterList, parameters: ParameterList, returnType: ReturnType, isDef: Boolean) = initValOrDef

        override def apply(document: IDocument): Unit = {
          for {
            //we must open the editor before doing the refactoring on the compilation unit:
            theDocument <- EditorUtils.findOrOpen(ssf.workspaceFile)
          } {
            val scu = ssf.getCompilationUnit.asInstanceOf[ScalaCompilationUnit]
            val changes = scu.withSourceFile { (srcFile, compiler) =>
//              if (isDef) {
                val refactoring = new AddMethod { val global = compiler }
                refactoring.addMethod(srcFile.file, cl.name.decode, abstrMember.nameString, parameters, returnType, target) //if we're here, className should be defined because of the check in isApplicable
//              } else {
//                val refactoring = new AddVariable { val global = compiler }
////                val setter = abstrMember.setter(abstrMember.owner)
////                val isVar = if (setter != compiler.NoSymbol) setter.isSetter else false
//                refactoring.addVariable(sourceFile.file, cl.name.decode, abstrMember.nameString, false, returnType, target)
//              }
            } getOrElse Nil

            for (change <- changes) {
              val edit = new ReplaceEdit(change.from, change.to - change.from, change.text)
              edit.apply(theDocument)
            }

            //TODO: we should allow them to change parameter names and types by tabbing
            for (change <- changes.headOption) {
              val offset = change.from + change.text.lastIndexOf("???")
              EditorUtils.enterLinkedModeUi(List((offset, "???".length)), selectFirst = true)
            }
          }
        }

        override def getDisplayString(): String = {
          val prettyParameterList = (for (parameterList <- parameters) yield {
            parameterList.map(_._2).mkString(", ")
          }).mkString("(", ")(", ")")

          val typeParametersList = if (!typeParameters.isEmpty) typeParameters.mkString("[",",","]") else ""

          val returnTypeStr = returnType.map(": " + _).getOrElse("")

          val base = s"Implement ${if (isDef) "def" else "val"} '${abstrMember.nameString}$typeParametersList$prettyParameterList$returnTypeStr'"
          base
        }

        override def getRelevance = 90
        override def getSelection(document: IDocument): Point = null
        override def getAdditionalProposalInfo(): String = null
        override def getImage(): Image = JavaPluginImages.DESC_MISC_PUBLIC.createImage()
        override def getContextInformation: IContextInformation = null
      }

      def implAbstractProposals(tree: NamedTree): List[IJavaCompletionProposal] =
        compiler.askOption { () =>
          val tp = tree.symbol.tpe
          (tp.members filter { m =>
//            (m.isMethod || m.isValue) && m.isIncompleteIn(tree.symbol) && m.isDeferred && !m.isSetter && (m.owner != tree.symbol)
            m.isMethod && m.isIncompleteIn(tree.symbol) && m.isDeferred && !m.isSetter && (m.owner != tree.symbol)
          }
            map { sym =>
              new AbstractMemberProposal(sym, tree, AddToClosest(offset))
            }).toList
        } getOrElse Nil

      def createPosition(sf: SourceFile, offset: Int) =
        compiler.rangePos(srcFile, offset, offset, offset)

      def enclosingClassOrModule(src: SourceFile, offset: Int) =
        compiler.locateIn(compiler.parseTree(src), createPosition(src, offset),
          t => (t.isInstanceOf[ClassDef] || t.isInstanceOf[ModuleDef]))

      val enclosing = enclosingClassOrModule(srcFile, offset)
      if (enclosing != EmptyTree) {
        compiler.withResponse[Tree] { response =>
          compiler.askTypeAt(enclosing.pos, response)
        }.get.left.toOption flatMap {
          case cd @ ClassDef(mods, name, tparams, impl) =>
            Option(implAbstractProposals(cd))
          case md @ ModuleDef(mods, name, impl) =>
            Option(implAbstractProposals(md))
          case _ => None
        } getOrElse (Nil)
      } else Nil
    } getOrElse (Nil)
  }
}