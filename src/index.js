'use strict';

require("./styles.scss");

const {Elm} = require('./Main');
var app = Elm.Main.init({flags: {session: localStorage.session || null}});

app.ports.storeSession.subscribe(function (session) {
    localStorage.session = session;
});

app.ports.writeToClipboard.subscribe(function (text) {
    navigator.clipboard.writeText(text).then(
        function () {
            app.ports.clipboardSuccess.send();
        },
        function () {
            app.ports.clipboardFailure.send();
        }
    );
});
