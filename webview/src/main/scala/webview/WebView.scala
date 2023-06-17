package webview

import scala.scalanative.unsafe.*
import CApi.*
import webview.WebView.{CBDesc, callbacks}

import scala.collection.mutable
import scala.scalanative.runtime.{Intrinsics, fromRawPtr, toRawPtr}

class WebView private (val w: webview_t) {
  def setTitle(title: String): Unit = Zone { implicit z =>
    webview_set_title(w, toCString(title))
  }
  def setHtml(html: String): Unit = Zone { implicit z =>
    webview_set_html(w, toCString(html))
  }
  def init(js: String): Unit = Zone { z =>
    webview_init(w, toCString(js)(z))
  }
  def run(): Unit =
    try webview_run(w)
    finally webview_destroy(w)

  def bind(name: String, f: String => String): CBDesc =
    WebView.callbacks.get(name).foreach(_.unbind())
    val handler = new WebView.CBDesc(name, f, w)
    WebView.callbacks += name -> handler
    Zone { implicit z =>
      webview_bind(w, toCString(name), WebView.bnd, fromRawPtr(Intrinsics.castObjectToRawPtr(handler)))
    }
    handler
}

object WebView {
  def apply() = new WebView(webview_create(1, null.asInstanceOf))

  // weird indirections because callbacks should not be GCed
  class CBDesc(name: String, val handler: String => String, val view: webview_t) {
    def unbind(): Unit = Zone { implicit z =>
      webview_unbind(view, toCString(name))
    }
  }
  val callbacks: mutable.Map[String, CBDesc] = mutable.Map.empty

  val bnd: BindCallback = { (seq: CString, req: CString, w: Ptr[Byte]) =>
    val cbdesc    = Intrinsics.castRawPtrToObject(toRawPtr(w)).asInstanceOf[CBDesc]
    val argstring = fromCString(req)
    val res       = cbdesc.handler(argstring)
    Zone { implicit z =>
      webview_return(cbdesc.view, seq, 0, toCString(res))
    }
  }
}
