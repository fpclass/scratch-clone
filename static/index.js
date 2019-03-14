$(document).ready(function() {
    Blockly.Blocks["math_change"] = null;
    Blockly.Blocks["entry_point"] = {
        init: function() {
            this.appendDummyInput().appendField("Start");
            this.setNextStatement(true, null);
            this.setColour(230);
            this.setTooltip("Main entry point to the program");
            this.setHelpUrl("");
        }
    };

    var blocklyDiv = $("main")[0];
    var workspace = Blockly.inject(blocklyDiv, {
        oneBasedIndex: false,
        media: "./static/google-blockly/media/",
        toolbox: $("#toolbox")[0]
    });

    Blockly.svgResize(workspace);

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
            addMessage(
                "Something has gone wrong. See the console for details."
            );
        });
    });
});
