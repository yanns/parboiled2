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

private[parboiled2] trait ParserMacroMethods { parser: Parser =>

  /** Converts a compile-time only rule definition into the corresponding rule method implementation.
    */
  inline def rule[I <: HList, O <: HList](inline r: Rule[I, O]): Rule[I, O] = ${ ParserMacros.ruleImpl('parser, 'r) }

  /** Converts a compile-time only rule definition into the corresponding rule method implementation
    * with an explicitly given name.
    */
  inline def namedRule[I <: HList, O <: HList](name: String)(inline r: Rule[I, O]): Rule[I, O] = ${
    ParserMacros.nameRuleImpl('parser, 'name, 'r)
  }

}

private[parboiled2] trait RuleRunnable {

  /** THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
    */
  implicit class Runnable[L <: HList](rule: RuleN[L]) {
    def run()(implicit scheme: Parser.DeliveryScheme[L]): scheme.Result = ???
  }
}

import scala.quoted._
class OpTreeContext(parser: Expr[Parser])(using Quotes) {
  sealed trait OpTree {
    def render(wrapped: Boolean): Expr[Boolean]
  }
  sealed abstract class NonTerminalOpTree extends OpTree {
    def bubbleUp(e: Expr[org.parboiled2.Parser#TracingBubbleException], start: Expr[Int]): Expr[Nothing]

    // renders a Boolean Tree
    def render(wrapped: Boolean): Expr[Boolean] =
      if (wrapped) '{
        val start = $parser.cursor
        try ${ renderInner('start, wrapped) } catch {
          case e: org.parboiled2.Parser#TracingBubbleException => ${ bubbleUp('e, 'start) }
        }
      }
      else renderInner(Expr(-1) /* dummy, won't be used */, wrapped)

    // renders a Boolean Tree
    protected def renderInner(start: Expr[Int], wrapped: Boolean): Expr[Boolean]
  }

  sealed abstract class DefaultNonTerminalOpTree extends NonTerminalOpTree {
    def bubbleUp(e: Expr[org.parboiled2.Parser#TracingBubbleException], start: Expr[Int]): Expr[Nothing] = '{
      $e.bubbleUp($ruleTraceNonTerminalKey, $start)
    }
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey]
  }
  sealed abstract class TerminalOpTree extends OpTree {
    def bubbleUp: Expr[Nothing] = '{ $parser.__bubbleUp($ruleTraceTerminal) }
    def ruleTraceTerminal: Expr[RuleTrace.Terminal]

    final def render(wrapped: Boolean): Expr[Boolean] =
      if (wrapped) '{
        try ${ renderInner(wrapped) } catch { case org.parboiled2.Parser.StartTracingException => $bubbleUp }
      }
      else renderInner(wrapped)

    protected def renderInner(wrapped: Boolean): Expr[Boolean]
  }

  case class Sequence(ops: Seq[OpTree]) extends OpTree {
    override def render(wrapped: Boolean): Expr[Boolean] =
      ops
        .map(_.render(wrapped))
        .reduceLeft((l, r) => '{ val ll = $l; if (ll) $r else false })
  }

  case class CharMatch(charTree: Expr[Char]) extends TerminalOpTree {
    def ruleTraceTerminal = '{ org.parboiled2.RuleTrace.CharMatch($charTree) }
    override def renderInner(wrapped: Boolean): Expr[Boolean] = {
      val unwrappedTree = '{
        $parser.cursorChar == $charTree && $parser.__advance()
      }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }

  case class StringMatch(stringTree: Expr[String]) extends OpTree {
    final private val autoExpandMaxStringLength = 8

    override def render(wrapped: Boolean): Expr[Boolean] =
      // TODO: add optimization for literal constant
      // def unrollUnwrapped(s: String, ix: Int = 0): Tree =
      //   if (ix < s.length) q"""
      //     if (cursorChar == ${s charAt ix}) {
      //       __advance()
      //       ${unrollUnwrapped(s, ix + 1)}:Boolean
      //     } else false"""
      //   else q"true"
      // def unrollWrapped(s: String, ix: Int = 0): Tree =
      //   if (ix < s.length) {
      //     val ch = s charAt ix
      //     q"""if (cursorChar == $ch) {
      //       __advance()
      //       __updateMaxCursor()
      //       ${unrollWrapped(s, ix + 1)}
      //     } else {
      //       try __registerMismatch()
      //       catch {
      //         case org.parboiled2.Parser.StartTracingException =>
      //           import org.parboiled2.RuleTrace._
      //           __bubbleUp(NonTerminal(StringMatch($stringTree), -$ix) :: Nil, CharMatch($ch))
      //       }
      //     }"""
      //   } else q"true"

      // stringTree match {
      //   case Literal(Constant(s: String)) if s.length <= autoExpandMaxStringLength =>
      //     if (s.isEmpty) q"true" else if (wrapped) unrollWrapped(s) else unrollUnwrapped(s)
      //   case _ =>
      //     if (wrapped) q"__matchStringWrapped($stringTree)"
      //     else q"__matchString($stringTree)"
      // }
      if (wrapped) '{ $parser.__matchStringWrapped($stringTree) }
      else '{ $parser.__matchString($stringTree) }
  }

  case class Capture(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = '{ RuleTrace.Capture }
    def renderInner(start: Expr[Int], wrapped: Boolean): Expr[Boolean] =
      '{
        val start1  = ${ if (wrapped) start else '{ $parser.cursor } }
        val matched = ${ op.render(wrapped) }
        if (matched) {
          $parser.valueStack.push($parser.input.sliceString(start1, $parser.cursor))
          true
        } else false
      }
  }

  def deconstruct(rule: Expr[Rule[_, _]]): OpTree = {
    import quotes.reflect.*
    def rec(rule: Term): OpTree = rule.asExprOf[Rule[_, _]] match {
      case '{ ($p: Parser).ch($c) }  => CharMatch(c)
      case '{ ($p: Parser).str($s) } => StringMatch(s)
      case x                         =>
        // These patterns cannot be parsed as quoted patterns because of the complicated type applies
        x.asTerm.underlyingArgument match {
          case Apply(Apply(TypeApply(Select(lhs, "~"), _), List(rhs)), _) =>
            Sequence(Seq(rec(lhs), rec(rhs)))
          case Apply(Apply(TypeApply(Select(_, "capture"), _), List(arg)), _) =>
            Capture(rec(arg))
          case _ => reportError(s"Invalid rule definition: [$x]", x)
        }
    }

    rec(rule.asTerm)
  }

  private def reportError(error: String, expr: Expr[Any])(using quotes: Quotes): Nothing = {
    quotes.reflect.report.error(error, expr)
    throw new Exception(error)
  }
}

object ParserMacros {
  import scala.quoted._
  import scala.compiletime._

  def ruleImpl[I <: HList: Type, O <: HList: Type](parser: Expr[Parser], r: Expr[Rule[I, O]])(using
      Quotes
  ): Expr[Rule[I, O]] = {
    import quotes.reflect.*
    nameRuleImpl(parser, Expr(Symbol.spliceOwner.owner.name), r)
  }

  def nameRuleImpl[I <: HList: Type, O <: HList: Type](parser: Expr[Parser], name: Expr[String], r: Expr[Rule[I, O]])(
      using Quotes
  ): Expr[Rule[I, O]] = {
    import quotes.reflect.*

    val ctx    = new OpTreeContext(parser)
    val opTree = ctx.deconstruct(r)

    '{
      def wrapped: Boolean = ${ opTree.render(wrapped = true) }
      val matched =
        if ($parser.__inErrorAnalysis) wrapped
        else ${ opTree.render(wrapped = false) }
      if (matched) org.parboiled2.Rule.asInstanceOf[Rule[I, O]] else null
    }
  }
}
