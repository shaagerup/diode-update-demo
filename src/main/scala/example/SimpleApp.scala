package example


import diode.ActionResult.{NoChange, ModelUpdate}
import diode.react.{ReactConnector, ModelProxy}
import diode.{ModelRW, ActionHandler, Circuit}
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom._

@JSExport("SimpleApp")
object SimpleApp extends JSApp {
  // create a view for the counter

  @JSExport
  override def main(): Unit = {
    val root = document.getElementById("root")
    val mountNode = root.asInstanceOf[org.scalajs.dom.Node]
    ReactDOM.render(AppCircuit.connect(x => x)(p => <.div(ModelC(ModelC.Props(p, Nil)), p().toString)), mountNode)
  }

}
case class Model(
  value : String,
  children : List[Model]
)

// Actions:
case class AddChild(path : List[Int])
case class RemoveChild(path : List[Int])
case class SetValue(path : List[Int], value : String)

object AppCircuit extends Circuit[Model] with ReactConnector[Model]  {
  // define initial value for the application model
  def initialModel = Model("", Nil)

  val handler = new ActionHandler[Model, Model](zoomRW(identity)((_,t) => t)) {
    def zoomToChild(path : List[Int], rw : ModelRW[Model,Model]) : Option[ModelRW[Model,Model]] = path match {
      case Nil => Some(rw)
      case i :: is if i < rw.value.children.length => zoomToChild(is, rw.zoomRW(s => s.children(i))((s,t) => s.copy(children = s.children.updated(i,t))))
      case _ => None
    }
    override def handle = {
      case a@AddChild(path) => {
        println(a)
        zoomToChild(path, modelRW).map{rw =>
          ModelUpdate(rw.updated(rw.value.copy(children = rw.value.children :+ Model("",Nil))))
        }.getOrElse{
          NoChange
        }
      }
      case a@RemoveChild(path) => {
        println(a)

        if (path.length == 0)
          NoChange
        else
          zoomToChild(path.init, modelRW).map { rw =>
            println("rw.updated = " + rw.updated(rw.value.copy(children = rw.value.children.splitAt(path.last) match {
              case (xs, y :: ys) => xs ++ ys
              case (xs, Nil) => xs
            })))
            ModelUpdate(rw.updated(rw.value.copy(children = rw.value.children.splitAt(path.last) match {
              case (xs, y :: ys) => xs ++ ys
              case (xs, Nil) => xs
            })))
          }.getOrElse{
            NoChange
          }
      }
      case a@SetValue(path, value) => {
        println(a)
        zoomToChild(path, modelRW).map{ rw =>
          ModelUpdate(rw.updated(rw.value.copy(value = value)))
        }.getOrElse{
          NoChange
        }

      }
    }
  }

  override val actionHandler = composeHandlers(handler)
}


object ModelC {

  case class Props(proxy : ModelProxy[Model], path : List[Int])


  class Backend($ : BackendScope[Props, Unit]) {
    val setText = ReusableFn( (path : List[Int], value : String) => SetValue(path,value) : AnyRef)

    def render(p : Props) : ReactElement = {
        <.div(
          p.proxy.connect(_.value)(pxy => TextInputC(TextInputC.Props(pxy, setText(p.path)))),
          <.button(^.cls := "fa fa-plus", p.path.toString, ^.onClick --> p.proxy.dispatch(AddChild(p.path))),
          <.ul(
            p.proxy.value.children.zipWithIndex.map{ case (m,i) =>
              <.li(^.key := i,
                <.button(^.cls := "fa fa-minus", ^.onClick --> p.proxy.dispatch(RemoveChild(p.path :+ i))),
                p.proxy.connect(m => m.children(i)){ pxy => ModelC(ModelC.Props(pxy, p.path :+ i)) }
              )
            }
          )
        )
    }
  }

  val component = ReactComponentB[Props]("ModelC")
    .stateless
    .backend(new Backend(_))
    .renderBackend
    .build


  def apply(p : Props) = component(p)
}





object TextInputC {
  case class Props(
                    proxy : ModelProxy[String],
                    onChangeDispatch : String ~=> AnyRef,
                    placeholder : String = "",
                    disabled : Boolean = false
                  )

  class Backend($ : BackendScope[Props, Unit]) {
    def render(p : Props) : ReactElement = {
      val v = p.proxy()
      <.input(
        ^.cls := "form-control",
        ^.tpe := "text",
        ^.value := v,
        ^.onChange ==> {e : ReactEventI => p.proxy.dispatch(p.onChangeDispatch(e.target.value)) },
        ^.placeholder := p.placeholder,
        ^.disabled := p.disabled
      )
    }
  }


  val component = ReactComponentB[Props]("TextInputC")
    .stateless
    .backend(new Backend(_))
    .renderBackend
    .build


  def apply(p : Props) = component(p)

}
