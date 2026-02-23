package mathlify

import com.raquo.laminar.api.L._
import com.raquo.laminar.domapi.DomApi
import com.raquo.laminar.utils.UnitSpec


class DomApiSpec extends UnitSpec {

  color.rgb(200, 100, 0, 0.5).value == "rgb(200 100 0 / 0.5)"

  it("HTML: parses") {
    expectNode(
      Mathlify.divNode,
      div of (
        className is "foo bar",
        "Hello ",
        b of "world"
      )
    )
  }
}