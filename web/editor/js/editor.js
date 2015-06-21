$(function () {

var createTable = function() {
    var table = $('<table>'),
        rowNum = $('#rowNum').val(),
        colNum = $('#colNum').val();

    console.log(rowNum);
    console.log(colNum);
    for (var row = 0; row < rowNum; row++) {
        var tr = $('<tr>')
        for (var col = 0; col < colNum; col++) {
            var cellId = row * colNum + col;
            tr.append("<td id=\"c" + cellId + "\">a</td>");
        }
        table.append(tr);
    }
    $('#field').append(table);
}

$('#createTableButton').click(createTable);

});
