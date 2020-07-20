const {app, session, BrowserWindow} = require('electron');

// Handle creating/removing shortcuts on Windows when installing/uninstalling.
if (require('electron-squirrel-startup')) { // eslint-disable-line global-require
    app.quit();
}

const createWindow = () => {
    // Set CSP Header
    const defaultCsp =
        ['default-src \'none\'',
            'script-src-elem \'self\'',
            'font-src \'self\'',
            'style-src-elem \'unsafe-inline\'',
            'connect-src *'
        ];

    const csp = process.env.npm_lifecycle_event === "start"
        ? [...defaultCsp, 'script-src  \'unsafe-eval\'']
        : defaultCsp;

    session.defaultSession.webRequest.onHeadersReceived((details, callback) => {
        if (details.url.startsWith('devtools:')) {
            callback({});
        } else {
            callback({
                responseHeaders: {
                    ...details.responseHeaders,
                    'Content-Security-Policy': [csp.join(';')]

                }
            })
        }
    });

    // Create the browser window.
    const mainWindow = new BrowserWindow({
        width: 1024,
        height: 768,
    });

    // and load the index.html of the app.
    mainWindow.loadURL(MAIN_WINDOW_WEBPACK_ENTRY);

    // Open the DevTools.
    if (process.env.npm_lifecycle_event === "start") {
        mainWindow.webContents.openDevTools();
    }
};

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow);

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') {
        app.quit();
    }
});

app.on('activate', () => {
    // On OS X it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) {
        createWindow();
    }
});

// Disable navigation
// https://www.electronjs.org/docs/tutorial/security#12-disable-or-limit-navigation
app.on('web-contents-created', (event, contents) => {
    contents.on('will-navigate', (event, navigationUrl) => {
        event.preventDefault()
    })
})
