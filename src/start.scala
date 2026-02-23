package mathlify

import com.raquo.laminar.domapi.DomApi

@main def compileMe = println("boo")

def divNode = DomApi.unsafeParseHtmlString("<div class='foo bar'>Hello <b>world</b></div>")