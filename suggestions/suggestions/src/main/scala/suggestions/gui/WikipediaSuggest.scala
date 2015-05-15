package suggestions
package gui

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import scala.util.{ Try, Success, Failure }
import scala.swing.event._
import swing.Swing._
import javax.swing.UIManager
import Orientation._
import rx.subscriptions.CompositeSubscription
import rx.lang.scala.Observable
import rx.lang.scala.Subscription
import observablex._
import search._

object WikipediaSuggest extends SimpleSwingApplication with ConcreteSwingApi with ConcreteWikipediaApi {

  {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    } catch {
      case t: Throwable =>
    }
  }

  def top = new MainFrame {

    /* gui setup */

    title = "Query Wikipedia"
    minimumSize = new Dimension(900, 600)

    val button = new Button("Get") {
      icon = new javax.swing.ImageIcon(javax.imageio.ImageIO.read(this.getClass.getResourceAsStream("/suggestions/wiki-icon.png")))
    }
    val searchTermField = new TextField
    val suggestionList = new ListView(ListBuffer[String]())
    val status = new Label(" ")
    val editorpane = new EditorPane {
      import javax.swing.border._
      border = new EtchedBorder(EtchedBorder.LOWERED)
      editable = false
      peer.setContentType("text/html")
    }

    contents = new BoxPanel(orientation = Vertical) {
      border = EmptyBorder(top = 5, left = 5, bottom = 5, right = 5)
      contents += new BoxPanel(orientation = Horizontal) {
        contents += new BoxPanel(orientation = Vertical) {
          maximumSize = new Dimension(240, 900)
          border = EmptyBorder(top = 10, left = 10, bottom = 10, right = 10)
          contents += new BoxPanel(orientation = Horizontal) {
            maximumSize = new Dimension(640, 30)
            border = EmptyBorder(top = 5, left = 0, bottom = 5, right = 0)
            contents += searchTermField
          }
          contents += new ScrollPane(suggestionList)
          contents += new BorderPanel {
            maximumSize = new Dimension(640, 30)
            add(button, BorderPanel.Position.Center)
          }
        }
        contents += new ScrollPane(editorpane)
      }
      contents += status
    }

    val eventScheduler = SchedulerEx.SwingEventThreadScheduler

    /**
     * Observables
     * You may find the following methods useful when manipulating GUI elements:
     *  `myListView.listData = aList` : sets the content of `myListView` to `aList`
     *  `myTextField.text = "react"` : sets the content of `myTextField` to "react"
     *  `myListView.selection.items` returns a list of selected items from `myListView`
     *  `myEditorPane.text = "act"` : sets the content of `myEditorPane` to "act"
     */

    // TO IMPLEMENT
    val searchTerms: Observable[String] = searchTermField.textValues

    // TO IMPLEMENT
    // Next, use the searchTerms observable to create an observable of lists of suggestions 
    // in which each list of suggestion corresponds to one search term. If any of the suggestion lists requests fail 
    // (make sure you don’t wait forever), we would like to have the throwable to print the error message, 
    // so we wrap the result into a Try. Use the methods defined earlier in the WikipediaApi:
    val suggestions: Observable[Try[List[String]]] = searchTerms.timedOut(10).concatRecovered( term => wikiSuggestResponseStream(term) )

    // TO IMPLEMENT
    // The suggestions observable should now be updated while you type. Problem is – there is no way to see these changes yet in the UI! 
    // To display them, we need to update the contents of a component called suggestionList every time the 
    // observable produces a value. If the suggestions value is not successful, we must print the error message into the 
    // status label. Use the subscribe method on suggestions to do this:
    val suggestionSubscription: Subscription =  suggestions.observeOn(eventScheduler) subscribe {
      x => x match {
        case Success(list) => suggestionList.listData = list
        case Failure(e) => status.text = e.getMessage
      }
    }

    // TO IMPLEMENT
    // Your next task will be to obtain an observable (selections) of button clicks that contains the search terms selected 
    // in the suggestion list at the time the button was clicked. If the suggestion list had no items selected, 
    // then the click should not be part of selections.
    //TODO
    val selections: Observable[String] = button.clicks.filter(_ => !suggestionList.listData.isEmpty).map { _ => suggestionList.selection.items.head }

    // TO IMPLEMENT
    val pages: Observable[Try[String]] = selections.concatRecovered( term => wikiPageResponseStream(term))

    // TO IMPLEMENT
    val pageSubscription: Subscription = pages.observeOn(eventScheduler) subscribe {
      x => x match {
        case Success(p) => editorpane.text = p
        case Failure(e) => println(e)
      }
    }

  }

}


trait ConcreteWikipediaApi extends WikipediaApi {
  def wikipediaSuggestion(term: String) = Search.wikipediaSuggestion(term)
  def wikipediaPage(term: String) = Search.wikipediaPage(term)
}


trait ConcreteSwingApi extends SwingApi {
  type ValueChanged = scala.swing.event.ValueChanged
  object ValueChanged {
    def unapply(x: Event) = x match {
      case vc: ValueChanged => Some(vc.source.asInstanceOf[TextField])
      case _ => None
    }
  }
  type ButtonClicked = scala.swing.event.ButtonClicked
  object ButtonClicked {
    def unapply(x: Event) = x match {
      case bc: ButtonClicked => Some(bc.source.asInstanceOf[Button])
      case _ => None
    }
  }
  type TextField = scala.swing.TextField
  type Button = scala.swing.Button
}
