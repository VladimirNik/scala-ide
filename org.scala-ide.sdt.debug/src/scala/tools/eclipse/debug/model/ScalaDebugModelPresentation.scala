package scala.tools.eclipse.debug.model

import scala.tools.eclipse.debug.ScalaDebugger
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.IStatus
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.debug.core.model.IValue
import org.eclipse.debug.internal.ui.views.variables.IndexedVariablePartition
import org.eclipse.debug.ui.{ IValueDetailListener, IDebugUIConstants, IDebugModelPresentation, DebugUITools }
import org.eclipse.jdt.internal.ui.javaeditor.EditorUtility
import org.eclipse.ui.IEditorInput
import org.eclipse.jface.viewers.ILabelProviderListener
import scala.tools.eclipse.debug.async.AsyncStackFrame
import org.eclipse.debug.core.model.IStackFrame
import org.eclipse.debug.core.model.IVariable
import org.eclipse.debug.ui.IInstructionPointerPresentation
import org.eclipse.ui.IEditorPart
import org.eclipse.jface.text.source.Annotation
import org.eclipse.swt.graphics.Image
import org.eclipse.debug.internal.ui.DebugUIMessages
import org.eclipse.debug.internal.ui.InstructionPointerAnnotation

/** Utility methods for the ScalaDebugModelPresentation class
 *  This object doesn't use any internal field, and is thread safe.
 */
object ScalaDebugModelPresentation {
  def computeDetail(value: IValue): String = {
    value match {
      case v: ScalaPrimitiveValue =>
        v.getValueString
      case v: ScalaStringReference =>
        v.underlying.value
      case v: ScalaNullValue =>
        "null"
      case arrayReference: ScalaArrayReference =>
        computeDetail(arrayReference)
      case objecReference: ScalaObjectReference =>
        computeDetail(objecReference)
      case _ =>
        ???
    }
  }

  /** Return the a toString() equivalent for an Array
   */
  private def computeDetail(arrayReference: ScalaArrayReference): String = {
    import scala.collection.JavaConverters._
    // There's a bug in the JDI implementation provided by the JDT, calling getValues()
    // on an array of size zero generates a java.lang.IndexOutOfBoundsException
    val array = arrayReference.underlying
    if (array.length == 0) {
      "Array()"
    } else {
      array.getValues.asScala.map(value => computeDetail(ScalaValue(value, arrayReference.getDebugTarget()))).mkString("Array(", ", ", ")")
    }
  }

  /** Return the value produced by calling toString() on the object.
   */
  private def computeDetail(objectReference: ScalaObjectReference): String = {
    try {
      objectReference.invokeMethod("toString", "()Ljava/lang/String;", ScalaDebugger.currentThreadOrFindFirstSuspendedThread(objectReference)) match {
        case s: ScalaStringReference =>
          s.underlying.value
        case n: ScalaNullValue =>
          "null"
      }
    } catch {
      case e: Exception =>
        "exception while invoking toString(): %s\n%s".format(e.getMessage(), e.getStackTraceString)
    }
  }

}

/** Generate the elements used by the UI.
 *  This class doesn't use any internal field, and is thread safe.
 */
class ScalaDebugModelPresentation extends IDebugModelPresentation with IInstructionPointerPresentation {

  // Members declared in org.eclipse.jface.viewers.IBaseLabelProvider

  override def addListener(listener: ILabelProviderListener): Unit = ???
  override def dispose(): Unit = {} // TODO: need real logic
  override def isLabelProperty(element: Any, property: String): Boolean = ???
  override def removeListener(listener: ILabelProviderListener): Unit = ???

  // Members declared in org.eclipse.debug.ui.IDebugModelPresentation

  override def computeDetail(value: IValue, listener: IValueDetailListener): Unit = {
    new Job("Computing Scala debug details") {
      override def run(progressMonitor: IProgressMonitor): IStatus = {
        // TODO: support error cases
        listener.detailComputed(value, ScalaDebugModelPresentation.computeDetail(value))
        Status.OK_STATUS
      }
    }.schedule()
  }

  override def getImage(element: Any): org.eclipse.swt.graphics.Image = {
    element match {
      case target: ScalaDebugTarget =>
        // TODO: right image depending of state
        DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_DEBUG_TARGET)
      case thread: ScalaThread =>
        // TODO: right image depending of state
        DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_THREAD_RUNNING)
      case stackFrame: ScalaStackFrame =>
        // TODO: right image depending of state
        DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_STACKFRAME)
      case variable: IVariable =>
        // TODO: right image depending on ?
        DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_VARIABLE)
      case variable: IndexedVariablePartition =>
        // variable used to split large arrays
        // TODO: see ScalaVariable before
        DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_VARIABLE)
      case asyncSF: IStackFrame =>
        // TODO: right image depending of state
        DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_STACKFRAME)
    }
  }

  override def getText(element: Any): String = {
    element match {
      case target: ScalaDebugTarget =>
        target.getName // TODO: everything
      case thread: ScalaThread =>
        getScalaThreadText(thread)
      case stackFrame: ScalaStackFrame =>
        getScalaStackFrameText(stackFrame)
      case _ => element.toString
    }
  }

  /** Currently we don't support any attributes. The standard one,
   *  `show type names`, might get here but we ignore it.
   */
  override def setAttribute(key: String, value: Any): Unit = {}

  // Members declared in org.eclipse.debug.ui.ISourcePresentation

  override def getEditorId(input: IEditorInput, element: Any): String = {
    EditorUtility.getEditorID(input)
  }

  override def getEditorInput(input: Any): IEditorInput = {
    EditorUtility.getEditorInput(input)
  }

  // ----

  /*
   * TODO: add support for thread state (running, suspended at ...)
   */
  def getScalaThreadText(thread: ScalaThread): String = {
    if (thread.isSystemThread)
      "Daemon System Thread [%s]".format(thread.getName)
    else
      "Thread [%s]".format(thread.getName)
  }

  /*
   * TODO: support for missing line numbers
   */
  def getScalaStackFrameText(stackFrame: ScalaStackFrame): String = {
    "%s line: %s".format(stackFrame.getMethodFullName, {
      val lineNumber = stackFrame.getLineNumber
      if (lineNumber == -1) {
        "not available"
      } else {
        lineNumber.toString
      }
    })
  }

  // from InstructionPointer
  def getInstructionPointerAnnotation(editorPart: IEditorPart, frame: IStackFrame): Annotation = {
    new InstructionPointerAnnotation(frame,
      IDebugUIConstants.ANNOTATION_TYPE_INSTRUCTION_POINTER_SECONDARY,
      DebugUIMessages.InstructionPointerAnnotation_1,
      DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_INSTRUCTION_POINTER))
  }

  /** Returns an identifier of a <code>org.eclipse.ui.editors.annotationTypes</code> extension used for
   *  the specified stack frame in the specified editor, or <code>null</code> if a default annotation
   *  should be used.
   *
   *  @param editorPart the editor the debugger has opened
   *  @param frame the stack frame for which the debugger is displaying
   *  source
   *  @return annotation type identifier or <code>null</code>
   */
  def getInstructionPointerAnnotationType(editorPart: IEditorPart, frame: IStackFrame): String =
    IDebugUIConstants.ANNOTATION_TYPE_INSTRUCTION_POINTER_SECONDARY

  /** Returns the instruction pointer image used for the specified stack frame in the specified
   *  editor, or <code>null</code> if a default image should be used.
   *  <p>
   *  By default, the debug platform uses different images for top stack
   *  frames and non-top stack frames in a thread.
   *  </p>
   *  @param editorPart the editor the debugger has opened
   *  @param frame the stack frame for which the debugger is displaying
   *  source
   *  @return image or <code>null</code>
   */
  def getInstructionPointerImage(editorPart: IEditorPart, frame: IStackFrame): Image =
    DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_INSTRUCTION_POINTER)

  /** Returns the text to associate with the instruction pointer annotation used for the
   *  specified stack frame in the specified editor, or <code>null</code> if a default
   *  message should be used.
   *  <p>
   *  By default, the debug platform uses different images for top stack
   *  frames and non-top stack frames in a thread.
   *  </p>
   *  @param editorPart the editor the debugger has opened
   *  @param frame the stack frame for which the debugger is displaying
   *  source
   *  @return message or <code>null</code>
   */
  def getInstructionPointerText(editorPart: IEditorPart, frame: IStackFrame): String =
    DebugUIMessages.InstructionPointerAnnotation_1
}
