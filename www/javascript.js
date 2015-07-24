$(document).ready(function() {
  $(".note").on('click', 'button.makeNote', function() {
    $('#noteTrace').val($(this).attr('id'));
    $(".addNoteToBoard").click();
    $(".drawNotes").click();
  });
  $(".note").on('click', 'button.remove-note', function(){
    $('#noteTrace').val($(this).attr('id'));
    $(".removeNote").click();
    $(".drawNotes").click();
  });
  $(".note").on('dblclick', 'h3.noteTitle', function(){
    var id = $(this).attr('id');
    $(this).replaceWith("<input type='text' class='editNoteTitle' id='" + id + "'></input>");
  });
  $(".note").on('keyup', 'input.editNoteTitle', function(e){
    if (e.which == 13) {
      $('#noteTrace').val($(this).attr('id') + ',' + $(this).val() );
      $('.editNoteTitleOnBoard').click();
    }
  });
  $(".note").on('dblclick', 'p.inNoteText', function() {
    alert("Paragraph was double clicked!");
    var id = $(this).attr('id');
    var content = $(this).text();
    $(this).replaceWith("<textArea class='editNoteText' id='" + id + "' rows='5'>" + content +"</textarea>");
  })
  $(".note").on('dblclick', 'textArea.editNoteText', function() {
    $('#noteTrace').val($(this).attr('id') + 'DELTA' + ($(this).val())).toString();
    $('.editNoteTextOnBoard').click();
  });
});
