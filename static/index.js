$(document).ready(function() {
    Blockly.Blocks["math_change"] = null;
    Blockly.Blocks["entry_point"] = {
        init: function() {
            this.appendDummyInput().appendField("Start");
            this.setNextStatement(true, null);
            this.setColour(230);
            this.setTooltip("Main entry point to the program");
            this.setHelpUrl("");
            this.setDeletable(false);
        }
    };

    var blocklyDiv = $("#blocklyDiv")[0];
    var workspace = Blockly.inject(blocklyDiv, {
        oneBasedIndex: false,
        media: "./static/google-blockly/media/",
        toolbox: $("#toolbox")[0]
    });

    Blockly.Xml.domToWorkspace(workspace, $("#template")[0]);

    var onchange = function(e) {};

    workspace.addChangeListener(onchange);

    var $output = $("#output");

    function addMessage(message) {
        var $message = $('<div class="message"></div>');
        $message.text(message);

        $output.append($message);
    }

    $("#runButton").click(function() {
        $output.children().remove();

        var xml = Blockly.Xml.workspaceToDom(workspace);

        $.ajax({
            method: "POST",
            url: "./run/",
            contentType: "text/xml",
            dataType: "json",
            data: Blockly.Xml.domToText(xml)
        }).done(function(data) {
            if (data.memory) {
                if (data.memory.length == 0) {
                    addMessage("No variables to display.");
                } else {
                    $.each(data.memory, function(i, cell) {
                        var $cell = $('<div class="variable"></div>');
                        var $addr = $('<div class="name"></div>');
                        var $val = $('<div class="value"></div>');

                        $addr.text(cell[0]);
                        $val.text(cell[1]);

                        $cell.append($addr);
                        $cell.append($val);
                        $output.append($cell);
                    });
                }
            } else if (data.error) {
                addMessage("Something has gone wrong: " + data.error);
            } else {
                addMessage("Something has gone wrong.");
            }
        })
        .fail(function(data) {
            console.error(data);
            addMessage("Something has gone wrong. See the console for details.");
        });
    });

    $("#importButton").click(function() {
        if(!checkFileApiSupport()) {
            return;
        }

        $("#importExportBox").toggleClass("importing");
    });

    $("#importInput").on('change', function() {
        var reader = new FileReader();
        reader.addEventListener("loadend", function() {
            var xml = Blockly.Xml.textToDom(reader.result); console.log(xml);
            if (xml.ownerDocument.getElementsByTagName('parsererror').length > 0) {
                alert("Invalid file.");
            } else {
                Blockly.Xml.domToWorkspace(workspace, xml);
            }
            $("#importExportBox").toggleClass("importing");
        });
        reader.readAsText(document.getElementById('importInput').files[0]);
    });

    $("#exportButton").click(function() {
        if (!checkFileApiSupport()) {
            return;
        }

        var xml = Blockly.Xml.workspaceToDom(workspace);
        var blob = new Blob([ Blockly.Xml.domToText(xml) ], { type: 'application/octet-stream' });
        saveAs(blob, 'scratch-program.xml');
    });

    function checkFileApiSupport() {
        var isConformant = window.Blob && window.FileReader;
        if (!isConformant) {
            alert("Your browser does not support the HTML File API. Importing/exporting will not work.");
        }
        return isConformant;
    }
});
