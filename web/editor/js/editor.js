$(function () {

    var CHAR_CODE_OFFSET = 96;

    var createTable = function() {
        var table = $('<table>'),
            rowNum = $('#rowNum').val(),
            colNum = $('#colNum').val();

        for (var row = 0; row < rowNum; row++) {
            var tr = $('<tr>')
            for (var col = 0; col < colNum; col++) {
                var cellId = row * colNum + col;
                tr.append("<td id=\"c" + cellId + "\"><input type=\"number\"></td>");
            }
            table.append(tr);
        }
        $('#field').append(table);
    }

    var changeButtonToSave = function () {
        $(this).text('Save');
        $(this).attr('id', 'saveButton');
        $(this).off('click');
        $(this).on('click', save);
    }

    var save = function () {
        var rowNum = $('#rowNum').val(),
            colNum = $('#colNum').val();

        var text = "";
        for (var row = 0; row < rowNum; row++) {
            for (var col = 0; col < colNum; col++) {
                var cellId = row * colNum + col,
                    value = $('#c' + cellId + " > input").val();
                if (value == "") {
                    text += ".";
                } else {
                    text += String.fromCharCode(Number(value) + CHAR_CODE_OFFSET);
                }
            }
            text += "<br>"
        }
        $('#output').append(text);
        $('#load').removeClass('disabled');
        $('#load').on('click', load);
    }

    var load = function() {
        var table = $('<table>'),
            data = $('#output').text(),
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
        $('#field > table').remove()
        $('#field').append(table);
    }

    $('#createTableButton').on('click', createTable);
    $('#createTableButton').on('click', changeButtonToSave);

});
