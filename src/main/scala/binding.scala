
import com.thoughtworks.binding.Binding.{BindingSeq, Constants, Var => VarBinding}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, Node, raw}

import scala.language.implicitConversions
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Success, Try}
import scalaz.Scalaz._

object ui {

  // aid to IDE for syntax hightlighting XML literal syntax
  implicit def node2dom[T<:raw.Node](x: xml.Node): Binding[T] = ???
  implicit def nodeBuffer2dom[T<:raw.Node](x: xml.NodeBuffer): Binding[BindingSeq[T]] = ???

  sealed trait Page
  case object mainPage extends Page
  case object readmePage extends Page
  case object macroPage extends Page

  val activePage :VarBinding[Page] = VarBinding(readmePage)

  val defaultInput = "I"

  val txt = VarBinding(defaultInput)

  val parsedTxt : Binding[Try[Expression]] = Binding{
    helper.parseTry(txt.bind)
  }
  parsedTxt.unwatch()

  val normalInp: VarBinding[Option[Expression]] = VarBinding(None)
  val normalised: Binding[Option[Expression]] = Binding{
    normalInp.bind.map(expr => expr.normalForm)
  }
  normalised.watch()

  def comment(str: String) = {
    str.replaceAll("(^|\\n)(?!#)", "$1# ")
  }

  val colourClass = s"blue-grey"
  val buttonClass = s"waves-effect waves-light $colourClass btn"

  // todo: invent better examples
  def examples = List(
    "Addition" -> "PLUS C3 C4",
    "Subtraction" -> "SUB C11 (SUB C5 C2)",
    "Fibonacci" -> "FIBONACCI C6",
    "SKI-in-IOTA" -> "(IOTA(IOTA(IOTA(IOTA IOTA)))) (IOTA(IOTA(IOTA IOTA))) (IOTA IOTA)",
    "Macro" -> "FACTORIAL=Y(λfx.ISZERO x C1 (MUL x (f (PRED x))));\nFACTORIAL C3",
    "Boolean" -> "XOR TRUE (AND TRUE FALSE)"
  )

  @dom val renderExamples = {
    <div>
      <em>Examples</em>:
        {for { sample <- Constants(examples: _*)} yield
          <a href="#" onclick={_:Event => txt.value = sample._2}>{sample._1} </a>
    }</div>
  }

  @dom val renderTxt = {

    <div>
    <h4>Input</h4>

      { renderExamples.bind }
      <label for="inp">Lambda Calculus Expression</label>
        <textarea id="inp"
                  oninput={evt: Event => txt.value = inp.value}
                  value={txt.bind}
                  placeholder="enter a lambda calculus expression"
                  rows={10}
                  cols={60}/>


        <button class={buttonClass}
                onclick={_: Event => txt.value = comment(txt.value)}>Comment</button>
        <button class={buttonClass}
                onclick={_: Event =>
                  val cursorLoc = inp.selectionStart
                  txt.value = {
                    val tmpStr = new StringBuilder(txt.value)
                    tmpStr.insert(cursorLoc, "λ")
                    tmpStr.toString
                  }
                  inp.focus
                  inp.selectionStart = cursorLoc + 1
                  inp.selectionEnd = cursorLoc + 1}>Insert λ</button>
        <button class={buttonClass}
                onclick={_: Event => txt.value = defaultInput}>Reset</button>
      </div>
  }


  @dom val renderParse = {

    parsedTxt.bind match {
        case Success(expr) => {
            <div>
              <h4>Parsed</h4>
              <label for="parse-output">Parsed Expression</label>
              <textarea id="parse-output" readOnly={true}>{ expr.printable }</textarea>
              <br/>
              <button class={buttonClass}
                     value="Normal form"
                     onclick={_: Event => {
                       normalInp.value = Some(expr)
                     }}>Analyse</button>
            </div>
        }
        case Failure(ex) =>
          <div>
            <h4>Parse Error</h4>
            <pre class="deep-orange-text">{ ex.getMessage }</pre>
          </div>
      }
  }

  @dom val renderNormalised = {
    normalised.bind match {
      case Some(expr) => {
        val ski = expr.eliminateAbstraction.printable
        <div>
        <h4>Analysis</h4>

          <label for="normalised-output">Head Normal Form</label>
          <textarea id="normalised-output" readOnly={true} class="low">{ expr.printable }</textarea>
          <a href="#"
                  onclick={_: Event =>
                    txt.value = comment(txt.value) + "\n" + expr.printable}>Append to Input</a>

          <br />
          <label for="ski-output">Abstractions eliminated</label>
          <textarea id="ski-output" readOnly={true} class="low">{ ski }</textarea>
          <a href="#"
                  onclick={_: Event =>
                    txt.value = comment(txt.value) + "\n" + ski}>Append to Input</a>

          <table>
            <tr>
              <td>As numeral</td>
              <td>{arith.interpretAsInt(expr).map(_.toString).getOrElse("not a number")}</td>
            </tr>
            <tr>
              <td>As boolean</td>
              <td>{arith.interpretAsBool(expr).map(_.toString).getOrElse("not a boolean")}</td>
            </tr>
            <tr>
              <td>Free variables</td>
              <td><ul>{for {v <- expr.freeVars.toList} yield <li>{v.name}</li> }</ul></td>
            </tr>
          </table>
        </div>
      }
      case None => <div><br/><br/></div>//<!-- no analysis -->
    }
  }

  @dom
  def renderMacros = {
    <div>
      <table class="striped">
        <thead>
          <tr><td>Macro</td><td>Definition</td></tr>
        </thead>
        <tbody>
          {
          for {(k,v) <- Constants(macros.allMacros.toSeq.sortBy(_._1): _*)} yield
          <tr><td>{k}</td><td>{v}</td></tr>
          }
      </tbody>
      </table>
    </div>
  }

  @dom
  def renderNav = {
    <nav>
      <div class="nav-wrapper grey">
        <a href="#" class="brand-logo left">Lambda Calculus Studio</a>
        <ul  class="right">
          <li><a href="#" onclick={_:Event => activePage.value = readmePage}>Readme</a></li>
          <li><a href="#" onclick={_:Event => activePage.value = mainPage}>Interact</a></li>
          <li><a href="#" onclick={_:Event => activePage.value = macroPage}>Macros</a></li>
        </ul>
      </div>
    </nav>
  }

  @dom
  def renderReadme = {
    <div>

      <p>
        This is a toy interpreter for experimenting with lambda calculus.
        The interpreter runs in your browser and large computations may
        cause stack overflow.
      </p>

      <h4>Syntax</h4>
      <p>
       Most common formats are accepted. The lambda character can be entered
        as L, ^&nbsp;(circumflex), λ&nbsp;(Unicode 03bb) or \&nbsp;(backslash).
        A lambda expression may be preceded by any number of macro definitions
        separated by semicolon. Macros use capital letters (except L). Macros
        should not have free variables.
        Variables may be a single lower case letter, followed by any number
        of primes&nbsp;('). Comments start with #.
<pre class="grey lighten-3"># line comment
MACRO=\x.x;
BACKREF=MACRO MACRO;
PRIME=Lx'.x';
ARROW=^x->BACKREF;
λx.x BACKREF ARROW</pre>
      </p>

      <h4>Features</h4>
      <p>
        The interpreter performs beta reduction and eta conversion.
        </p>
      <p>
        The abstraction elimination uses the strategy suggested in
        <a href="https://tromp.github.io/cl/LC.pdf">this paper (§3.2)</a>.
      </p>

      <h4>Todo</h4>
      <ul>
        <li>Trace beta-reductions</li>
        <li>Cancel slow computations</li>
        <li>Unlambda and Lazy-k compatibility</li>
      </ul>

      <h4>References</h4>
      <ul>
        <li><a href="https://github.com/hallettj/LambdaCalculus">https://github.com/hallettj/LambdaCalculus</a></li>
        <li><a href="http://jwodder.freeshell.org/lambda.html">http://jwodder.freeshell.org/lambda.html"</a></li>
        <li><a href="https://github.com/lihaoyi/fastparse">https://github.com/lihaoyi/fastparse</a></li>
        <li><a href="https://www.scala-js.org/">https://www.scala-js.org/</a></li>
        <li><a href="https://github.com/ThoughtWorksInc/Binding.scala">https://github.com/ThoughtWorksInc/Binding.scala</a></li>
      </ul>
    </div>
  }

  @dom
  def renderMainPage: Binding[BindingSeq[Node]] = {
    <div>
      {renderTxt.bind}
    </div>
    <div>
      {renderParse.bind}
    </div>
    <div>
      {renderNormalised.bind}
    </div>
  }

  @dom
  def render = {
    <div>
      <div>{renderNav.bind}</div>
      <div>
        {
        activePage.bind match {
        case p if p == mainPage => <div>{ renderMainPage.bind }</div>
        case p if p == macroPage => <div>{ renderMacros.bind }</div>
        case p if p == readmePage => <div>{ renderReadme.bind }</div>
        case _ => <div>404 Not Found</div>
      }}</div>
    </div>
  }

  @JSExportTopLevel("main")
  def main(container: Node): Unit = {
    dom.render(container, render)
  }
}
