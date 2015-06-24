$(function () {

    var CHAR_CODE_OFFSET = 96;

    var load = function() {
        var table = $('<table>'),
            data = $('#input').text(),
            rowNum = $('#rowNum').val(),
            colNum = $('#colNum').val();

        for (var row = 0; row < rowNum; row++) {
            var tr = $('<tr>')
            for (var col = 0; col < colNum; col++) {
                var cellId = row * colNum + col,
                    c = data[cellId];
                if (c == '.') {
                    tr.append("<td id=\"c" + cellId + "\"></td>");
                } else {
                    tr.append("<td id=\"c" + cellId + "\">" + (c.charCodeAt() - CHAR_CODE_OFFSET) + "</td>");
                }
            }
            table.append(tr);
        }
        $('#field').append(table);
    }

    var start = function() {
    }

    load();

});
