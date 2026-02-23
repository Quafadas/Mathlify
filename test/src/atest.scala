package mathlify

import com.raquo.laminar.api.L._
import com.raquo.laminar.domapi.DomApi
import com.raquo.laminar.utils.UnitSpec


class DomApiSpec extends UnitSpec {

  color.rgb(200, 100, 0, 0.5).value == "rgb(200 100 0 / 0.5)"

  it("Correctly adds a node and tests some properties") {
    expectNode(
      mathlify.divNode,
      div of (
        className is "foo bar",
        "Hello ",
        b of "world"
      )
    )
  }
}