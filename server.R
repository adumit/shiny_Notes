#Shiny Notes by Andrew Dumit is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License.
#Based on a work at https://github.com/adumit/shiny_Notes.

library(shiny)
library(stringr)
library(leaflet)

load("data/noteData.rda")
load("data/pointers.rda")
load("data/noteBoardData.rda")
numNotes <- length(noteData)

shinyServer(function(input, output) {
  
  #Draw noteBoard and indexCard on startup
  output$indexCard <- renderUI({
    HTML(createIndex())
  })
  output$noteBoard <- renderUI ({
    HTML(drawNotes(1, 1))
  })
  
  #Regular functionality
  updateNotes <- observeEvent(input$ActionButton, {
    tempNote = list()
    tempNote$noteID = input$noteID
    tempNote$noteText = input$noteText
    refTo <- str_extract_all(input$noteText, "(\\{.+?\\})")[[1]]
    for (i in 1:length(refTo)) {
      refTo[i] = substr(refTo[i], 2, nchar(refTo[i]) - 1)
    }
    if (tempNote$noteID != "" & !(tempNote$noteID %in% names(noteData))) {
      updatePointers(refTo, input$noteID)
      noteData[input$noteID] <<- list(tempNote)
      numNotes <<- length(noteData)
    }
    save(pointers, file="data/pointers.rda")
    save(noteData, file="data/noteData.rda")
  })
  
  updatePointers <- function(refTo, noteID) {
    pointers <<- subset(pointers, pointers$from != noteID)
    for (i in 1:length(refTo)) {
      to = refTo[i]
      from = noteID
      num = i
      pointer = c(from, to, num)
      pointers[dim(pointers)[1] + 1,] <<- pointer
    }
  }
  
  createIndex <- function() {
    retText = ""
    for (i in names(noteData)) {
      if (i == "index") {
        next
      }
      retText = paste0(retText, '<li>', '<button id="',1, ",", i, '" type="button" class="makeNote btn btn-link shiny-bound-input">'
                       , i ,'</button></li>')
      
    }
    counter = 1
    for (i in names(noteData)) {
      counter = counter + 1
      fromOrder = counter
      to = i
      from = "index"
      num = counter
      pointer = c(from, to, fromOrder)
      pointers[dim(pointers)[1] + 1,] <<- pointer
    }
    noteData$index$noteID <<- "index"
    noteData$index$noteText <<- paste0("<ul>", retText, "</ul>")
    return(paste0("<ul style='color: black'>", retText, "</ul>"))
  }
  
  updateIndex <- observeEvent(input$ActionButton, {
    output$indexCard <- renderUI({
      HTML(createIndex())
    }) 
  })
  
  createNoteText <- function(noteTextStr, BIDnum) {
    retText = noteTextStr
    buttons <- str_extract_all(noteTextStr, "(\\{.+?\\})")[[1]]
    if (length(buttons) == 0) {
      return(retText)
    }
    for (i in 1:length(buttons)) {
      noLastLetter = substr(buttons[i], 1, nchar(buttons[i]) - 1)
      button = paste0("(\\", noLastLetter, "\\})")
      name = substr(buttons[i], 2, nchar(buttons[i]) - 1)
      retText <- str_replace(retText, button, 
                             paste0('<button id="',BIDnum, ',', name,'" type="button" class="makeNote btn btn-link shiny-bound-input">', name,'</button>'))
    }
    return(paste0("<p class='ref inNoteText' id=", BIDnum ,">",retText,"</p>"))
  }
  
  drawNotes <- function(BIDnum, level) {
    tempData = subset(noteBoardData, PBID == BIDnum)
    smallCol = 12/(5-level)
    largeCol = 12 - smallCol
    returnText = ""
    if (as.numeric(BIDnum) == 1) {
      if (dim(tempData)[1] == 0) {
        return()
      }
      else {
        for (i in 1:dim(tempData)[1]) {
          returnText = paste0(returnText, drawNotes(tempData[i,]$BID, level))
        }
      }
    }
    else if (dim(tempData)[1] == 0) {
      returnText = paste0(returnText,
                            '<div class="col-xs-', smallCol ,'">',
                              '<div class="box">',
                              '<div class="box-header"><div class="pull-right">
            <button class="btn remove-note"id="', paste0("d",BIDnum),'"><i class="fa fa-times"></i></button>
          </div><h3 class="box-title noteTitle" id="', BIDnum,'">',as.character(noteBoardData$noteID[noteBoardData$BID == BIDnum]),'</h3></div>',
                              '<div class="box-body note">',createNoteText(as.character(noteData[[as.character(noteBoardData$noteID[noteBoardData$BID == BIDnum])]]$noteText), BIDnum),'</div>',
                              '</div>', 
                            '</div>',
                            '<div class="col-xs-', largeCol ,'">',
                            '</div>')
    }
    else {
      tempText = ""
      for (i in 1:dim(tempData)[1]) {
        tempText = paste0(tempText, drawNotes(tempData[i,]$BID, level+1))
      }
      return(paste0('<div class="box-body">',
                    '<div class="col-xs-', smallCol ,'">',
                      '<div class="box">',
                      '<div class="box-header"><div class="pull-right">
            <button class="btn remove-note" id="', paste0("d",BIDnum),'"><i class="fa fa-times"></i></button>
          </div><h3 class="box-title noteTitle" id="', BIDnum,'">',as.character(noteBoardData$noteID[noteBoardData$BID == BIDnum]),'</h3></div>',
                      '<div class="box-body note">',createNoteText(as.character(noteData[[as.character(noteBoardData$noteID[noteBoardData$BID == BIDnum])]]$noteText), BIDnum),'</div>',
                      '</div>', 
                    '</div>',
                    '<div class="col-xs-', largeCol ,'">',
                      tempText,
                    '</div>', '</div>'))
    }
    return(returnText)
  }
  
  reactToDrawNotesButton <- observeEvent(input$drawNotes, {
    output$noteBoard <- renderUI ({
      HTML(drawNotes(1, 1))
    })
  })
  
  reactToAddNotesButton <- observeEvent(input$addNoteToBoard, {
    updateNoteBoardData(as.character(input$noteBoardData))
    output$noteBoard <- renderUI ({
      HTML(drawNotes(1, 1))
    })
  })
  
  removeNote <- function(noteRemove) {
    noteRem = substr(noteRemove, 2, nchar(noteRemove))
    tempData = subset(noteBoardData, PBID == as.numeric(as.character(noteRem)))
    if (dim(tempData)[1] != 0) {
      for (i in 1:dim(tempData)[1]) {
        removeNote(paste0("d", tempData[i,]$BID))
      }
    }
    noteBoardData <<- subset(noteBoardData, BID != as.numeric(as.character(noteRem)))
    save(noteBoardData, file="data/noteBoardData.rda")
  }
  
  reactToRemoveNote <- observeEvent(input$removeNote, {
    removeNote(input$noteBoardData)
    output$noteBoard <- renderUI ({
      HTML(drawNotes(1, 1))
    })
  })
  
  updateNoteBoardData <- function(noteTrace) {
    noteToAdd = noteTrace
    sepText = strsplit(noteToAdd, ",")
    BID = as.numeric(max(noteBoardData$BID)) + 1
    noteID = as.character(sepText[[1]][2])
    if (!(noteID %in% names(noteData))) {
      makeNewBlankNote(noteID)
      output$indexCard <- renderUI({
        HTML(createIndex())
      })
    }
    PBID =  as.numeric(sepText[[1]][1])
    tempRow = c(BID, noteID, PBID)
    nextRow = dim(noteBoardData)[1] + 1
    noteBoardData[nextRow,1] <<- BID
    noteBoardData[nextRow,2] <<- noteID
    noteBoardData[nextRow,3] <<- PBID
    save(noteBoardData, file="data/noteBoardData.rda")
  }
  
  makeNewBlankNote <- function(newNote) {
    tempData = list()
    tempData$noteID = newNote
    tempData$noteText = ""
    noteData[[newNote]] <<- tempData
    save(noteData, file="noteData.rda")
  }
  
  reactToEditNoteOnBoard <- observeEvent(input$editNoteTitleOnBoard, {
    editNoteTitleOnBoard(input$noteBoardData)
  })
  
  changeTitleOnNoteBoard <- function(noteID, title) {
    noteBoardData$noteID[noteBoardData$noteID == noteID] <<- title
  }
  
  changeTitleInNoteData <- function(oldID, newID) {
    elemToChange = which(names(noteData) == oldID)[[1]]
    names(noteData)[elemToChange] <<- newID
    noteData[[elemToChange]]$noteID <<- newID
  }
  
  changeTitleInPointers <- function(oldID, newID) {
    pointers$from[pointers$from == oldID] <<- newID
    pointers$to[pointers$to == oldID] <<- newID
  }
  
  replaceNoteTextButtons <- function(oldID, newID) {
    for (i in names(noteData)) {
      noteText = noteData[[i]]$noteText
      buttons = str_extract_all(noteData[[i]]$noteText, "(\\{.+?\\})")[[1]]
      if (length(buttons) == 0) {
        next
      }
      for (j in 1:length(buttons)) {
        curID = substr(buttons[j], 2, nchar(buttons[j]) - 1)
        if (curID == oldID) {
          oldButton = paste0("(\\{", oldID, "\\})")
          newButton = paste0("\\{", newID, "\\}")
          noteText <- str_replace(noteText, oldButton, newButton)
          noteData[[i]]$noteText <<- noteText
        }
      }
    }
  }
  
  editNoteTitleOnBoard <- function(newTitle) {
    sepList = strsplit(newTitle, ",")
    BIDtoEdit = as.numeric(sepList[[1]][1])
    titleChange = sepList[[1]][2]
    noteID = noteBoardData$noteID[noteBoardData$BID == BIDtoEdit]
    changeTitleInNoteData(noteID, titleChange)
    changeTitleOnNoteBoard(noteID, titleChange)
    replaceNoteTextButtons(noteID, titleChange)
    changeTitleInPointers(noteID, titleChange)
    output$indexCard <- renderUI({
      HTML(createIndex())
    })
    output$noteBoard <- renderUI ({
      HTML(drawNotes(1, 1))
    })
  }
  
  reactToEditNoteTextOnBoard <- observeEvent(input$editNoteTextOnBoard, {
    editNoteTextOnBoard(input$noteBoardData)
  })
  
  editNoteTextOnBoard <- function(inputText) {
    sepList = strsplit(inputText, "∆")
    BIDtoEdit = sepList[[1]][1]
    newText = sepList[[1]][2]
    noteID = noteBoardData$noteID[noteBoardData$BID == BIDtoEdit]
    editNoteTextData(noteID, newText)
    output$noteBoard <- renderUI ({
      HTML(drawNotes(1, 1))
    })
  }
  
  editNoteTextData <- function(noteID, newText) {
    elemToChange = which(names(noteData) == noteID)[[1]]
    noteData[[elemToChange]]$noteText <<- newText
  }
  
  resetNotes <- observeEvent(input$reset, {
    noteData = list()
    pointers = data.frame("from" = c(NA), "to" = c(NA), "fromOrder" = c(NA))
    pointers = pointers[-1,]
    noteBoardData = data.frame("BID" = 1, "noteID" = "index", "PBID" = 0)
    noteBoardData$BID = as.numeric(noteBoardData$BID)
    noteBoardData$PBID = as.numeric(noteBoardData$PBID)
    noteBoardData$noteID = as.character(noteBoardData$noteID)
    save(noteBoardData, file="data/noteBoardData.rda")
    save(pointers, file="data/pointers.rda")
    save(noteData, file="data/noteData.rda")
  })
  
  output$test <- renderText({
    as.character(input$noteBoardData)
  })
  
})
