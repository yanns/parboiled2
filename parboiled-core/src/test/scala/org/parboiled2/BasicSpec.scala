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

import utest.{TestableString => _, _}

object BasicSpec2 extends TestParserSpec {

  val tests = Tests {

    import utest.TestableString

    "The Parser should correctly recognize/reject input for" - {

      "simple char literals" - new TestParser0 {
        def targetRule = rule('x')
        "x" must beMatched
        "y" must beMismatched
        "" must beMismatched
      }
    }
  }
}
