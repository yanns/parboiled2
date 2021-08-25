/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2

import org.parboiled2.support.hlist.HList

private[parboiled2] trait ParserMacroMethods {

  /** Converts a compile-time only rule definition into the corresponding rule method implementation.
    */
  inline def rule[I <: HList, O <: HList](inline r: Rule[I, O]): Rule[I, O] = ${ ParserMacros.ruleImpl('r) }

  /** Converts a compile-time only rule definition into the corresponding rule method implementation
    * with an explicitly given name.
    */
  inline def namedRule[I <: HList, O <: HList](name: String)(inline r: Rule[I, O]): Rule[I, O] = ${
    ParserMacros.nameRuleImpl('name)('r)
  }

}

private[parboiled2] trait RuleRunnable {

  /** THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
    */
  implicit class Runnable[L <: HList](rule: RuleN[L]) {
    def run()(implicit scheme: Parser.DeliveryScheme[L]): scheme.Result = ???
  }
}

object ParserMacros {
  import scala.quoted._
  import scala.compiletime._

  def ruleImpl[I <: HList: Type, O <: HList: Type](r: Expr[Rule[I, O]])(using Quotes): Expr[Rule[I, O]] =
    nameRuleImpl(Expr("todo"))(r)

  def nameRuleImpl[I <: HList: Type, O <: HList: Type](
      name: Expr[String]
  )(r: Expr[Rule[I, O]])(using Quotes): Expr[Rule[I, O]] = {
    import quotes.reflect.*
    val opTree: OpTree = r match {
      case '{ ($p: Parser).ch($c) } =>
        CharMatch(p, c)
      case _ => reportError("does not understand", name)
    }

    val parser = opTree.parser
    '{
      def wrapped: Boolean = ${ opTree.render(wrapped = true) }
      val matched =
        if ($parser.__inErrorAnalysis) wrapped
        else ${ opTree.render(wrapped = false) }
      if (matched) org.parboiled2.Rule.asInstanceOf[Rule[I, O]] else null
    }
  }

  private def reportError(error: String, expr: Expr[Any])(using quotes: Quotes): Nothing = {
    quotes.reflect.report.error(error, expr)
    throw new Exception(error)
  }

  sealed trait OpTree {
    def parser: Expr[Parser]
    def render(wrapped: Boolean)(using Quotes): Expr[Boolean]
  }

  sealed abstract class TerminalOpTree(parser: Expr[Parser]) extends OpTree {
    def bubbleUp(using quotes: Quotes): Expr[Nothing] = '{ $parser.__bubbleUp($ruleTraceTerminal) }
    def ruleTraceTerminal(using quotes: Quotes): Expr[RuleTrace.Terminal]

    final def render(wrapped: Boolean)(using quotes: Quotes): Expr[Boolean] =
      if (wrapped) '{
        try ${ renderInner(wrapped) } catch { case org.parboiled2.Parser.StartTracingException => $bubbleUp }
      }
      else renderInner(wrapped)

    protected def renderInner(wrapped: Boolean)(using Quotes): Expr[Boolean]
  }

  case class CharMatch(parser: Expr[Parser], charTree: Expr[Char]) extends TerminalOpTree(parser) {
    def ruleTraceTerminal(using quotes: Quotes) = '{ org.parboiled2.RuleTrace.CharMatch($charTree) }
    override def renderInner(wrapped: Boolean)(using Quotes): Expr[Boolean] = {
      val unwrappedTree = '{
        $parser.cursorChar == $charTree && $parser.__advance()
      }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }
}
