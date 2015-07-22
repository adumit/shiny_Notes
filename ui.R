#Shiny Notes by Andrew Dumit is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License.
#Based on a work at https://github.com/adumit/shiny_Notes.

library(shiny)
library(shinydashboard)
source('server.R', local=TRUE)
library(leaflet)

dashboardPage(
  dashboardHeader(title = "Notes App"),
  dashboardSidebar(
    box(
      title = "Note ID",
      width = 12,
      tags$textarea(id="noteID", cols = 26, rows = 1, style="color: black")
    ),
    box(
      title = "Note Text",
      tags$textarea(id="noteText", cols = 26, rows = 5, style="color: black"),
      width = 12
    ),
    actionButton("ActionButton", "Create Note", class="center-block"),
    actionButton("drawNotes", "Draw Notes", class="center-block drawNotes"),
    actionButton("reset", "Clear Notes", class="center-block"),
    box(
      class = "note",
      title = "Index Note",
      width = 12,
      uiOutput("indexCard")
    ),
    HTML("<input type='text' class='noteTrace' style='visibility: hidden; color: black' id='noteTrace' value='as'>"),
    actionButton("addNoteToBoard", "Add Note to Board", class="center-block addNoteToBoard", style="visibility: hidden"),
    tags$script('document.getElementById("addNoteToBoard").onclick = function() {
                  var string = document.getElementById("noteTrace").value;
                  Shiny.onInputChange("noteBoardData", string);
                  };
                '),
    actionButton("removeNote", "Remove Notes", class="center-block removeNote", style="visibility:hidden"),
    tags$script('document.getElementById("removeNote").onclick = function() {
                  var string = document.getElementById("noteTrace").value;
                Shiny.onInputChange("noteBoardData", string);
                };
                '),
    actionButton("editNoteTitleOnBoard", "Edit Note on Board", class="center-block editNoteTitleOnBoard", style="visibility: hidden"),
    tags$script('document.getElementById("editNoteTitleOnBoard").onclick = function() {
                    var string = document.getElementById("noteTrace").value;
                    Shiny.onInputChange("noteBoardData", string);
                };
                '),
    actionButton("editNoteTextOnBoard", "Edit Note on Board", class="center-block editNoteTextOnBoard", style="visibility: hidden"),
    tags$script('document.getElementById("editNoteTextOnBoard").onclick = function() {
                var string = document.getElementById("noteTrace").value;
                Shiny.onInputChange("noteBoardData", string);
                };
                ')
    ),
  dashboardBody(
    fluidRow(
      box(
        width = 12,
        class = "note",
        includeScript("www/javascript.js"),
        includeScript("www/sendkeys.js"),
        uiOutput("noteBoard")
      )
    )
  )
)
