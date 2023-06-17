import webview.WebView

// based on
// https://github.com/lolgab/webview-scala


object Main {
  def main(args: Array[String]): Unit = {
    val w = WebView()
    w.setHtml(raw"""<h1 id="eins"> HALLO WELT </h1>""")
    w.init(raw"""
function callback() {
  const elem = document.getElementById("eins");
  elem.textContent = 10
  outpt(1)
}
setInterval("callback()",1000);
    """)
    val res = w.bind(
      "outpt",
      b => {
        println("received:")
        println(b)
        b
      }
    )
    w.run()
    println(res)
  }
}
