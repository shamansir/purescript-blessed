// const blessed = require('blessed');
// const fs = require('fs');
// const util = require('util');

// const blessed = require('blessed');
import blessed from '@shamansir/everblessed';
import terminalSize from 'terminal-size';
import * as fs from 'fs';
import * as util from 'util';

// console.log(blessedPkg);
// console.log(blessed);

let config =
    { blessedOn : true
    , loggingBlessedTo : null  // null | false | 'console' | <file-path>
    , loggingCommandsTo : null // null | false | 'console' | <file-path>
    };

let blessedLogFile = null;
let commandsLogFile = null;

const INIT_HANDLER_KEY = 'init';

let registry;
let handlersFns;

if (typeof blessed === 'undefined' || blessed === null) {
    console.error('chjj/BLESSED library is not installed');
    ___log_bl('chjj/BLESSED library is not installed');
}

// FIXME: move more logic to PureScript and for JS it should be just simple functions

function buildRecord(array, fn) {
    return array.reduce((record, item, idx) => {
        const pair = fn(item, idx);
        record[pair.name] = pair.value;
        return record;
    }, {});
}

function adaptProp(hs, parentProp) {
    return function(prop) {
        switch (prop.name) {
            case 'border':
                return { name : 'border', value : buildRecord(prop.value, adaptProp(hs, 'border')) };
            case 'style':
                return { name: 'style', value : buildRecord(prop.value, adaptProp(hs, 'style')) };
            case 'item':
                return { name: 'item', value : buildRecord(prop.value, adaptProp(hs, 'item')) };
            case 'selected':
                return { name: 'selected', value : buildRecord(prop.value, adaptProp(hs, 'selected')) };
            case 'hover':
                return { name: 'hover', value : buildRecord(prop.value, adaptProp(hs, 'hover')) };
            case 'focus':
                return { name: 'focus', value : buildRecord(prop.value, adaptProp(hs, 'focus')) };
            case 'parent':
                return { name: 'parent', value : prop.tag === 'Just' ? registry[prop.value].blessed : null };
            case 'commands':
                return { name: 'commands', value : buildRecord(prop.value, adaptListBarCommandValue(hs)) }
            default:
                return prop;
        }
    }
}

function execute(program) {
    return function() {
        ensureTTY();
        registry = {};
        handlersFns = buildRecord(program.handlersFns, (hdl) => ({ name : hdl.index, value : hdl }));

        registerNode(program.root)();
    }
}

function ensureTTY() {
    if (!process.stdout.isTTY) {
        const {columns, rows} = terminalSize();
        process.stdout.isTTY = true;
        process.stdout.columns = columns;
        process.stdout.rows = rows;
    }
}

function performInit(handlerDef) {
    let initEventIndex, initHandlerFn;
    if (handlerDef) {
        initEventIndex = handlerDef.index;
        initHandlerFn = handlersFns[initEventIndex];

        ___log_bl('calling ', initEventIndex);
        if (initHandlerFn && (initHandlerFn.index === initEventIndex)) {
            initHandlerFn.call()();
        }
    }
}

function bindHandler(blessedObj, handler) {
    if (handler.event === 'init') return;
    if (handler.event === 'command') return;
    const handlerFn = handlersFns[handler.index];

    ___log_bl('registering handler', handler.index, handler, handlerFn);
    if (config.blessedOn && handlerFn && (handlerFn.index === handler.index)) {
        if (handler.event === 'key') {
            blessedObj.key(handlerFn.args, (evt) => handlerFn.call(evt)());
        } else {
            blessedObj.on(handler.event, (evt) => handlerFn.call(evt)());
        }
    }
}

function registerNode(node) {
    return function() {
        ___log_bl('node', node);

        const handlers = buildRecord(node.handlers, (evt) => ({ name : evt.eventUniqueId, value : evt }));
        const props = buildRecord(node.props, adaptProp(handlers, null));

        ___log_bl('props', props);
        ___log_bl('handlers', handlers);

        let blessedObj = null;

        switch (node.nodeSubj) {
            // FIXME just call blessed[node.nodeSubj](props)]
            case 'node':
                if (!config.blessedOn) break;
                blessedObj = blessed.node(props);
                break;
            case 'screen':
                if (!config.blessedOn) break;
                blessedObj = blessed.screen(props);
                break;
            case 'element':
                if (!config.blessedOn) break;
                blessedObj = blessed.element(props);
                break;
            case 'box':
                if (!config.blessedOn) break;
                blessedObj = blessed.box(props);
                break;
            case 'text':
                if (!config.blessedOn) break;
                blessedObj = blessed.text(props);
                break;
            case 'line':
                if (!config.blessedOn) break;
                blessedObj = blessed.line(props);
                break;
            case 'bigtext':
                if (!config.blessedOn) break;
                blessedObj = blessed.bigtext(props);
                break;
            case 'list':
                if (!config.blessedOn) break;
                blessedObj = blessed.list(props);
                break;
            case 'filemanager':
                if (!config.blessedOn) break;
                blessedObj = blessed.filemanager(props);
                break;
            case 'listtable':
                if (!config.blessedOn) break;
                blessedObj = blessed.listtable(props);
                break;
            case 'listbar':
                if (!config.blessedOn) break;
                blessedObj = blessed.listbar(props);
                break;
            case 'form':
                if (!config.blessedOn) break;
                blessedObj = blessed.form(props);
                break;
            case 'input':
                if (!config.blessedOn) break;
                blessedObj = blessed.input(props);
                break;
            case 'textarea':
                if (!config.blessedOn) break;
                blessedObj = blessed.textarea(props);
                break;
            case 'textbox':
                if (!config.blessedOn) break;
                blessedObj = blessed.textbox(props);
                break;
            case 'button':
                if (!config.blessedOn) break;
                blessedObj = blessed.button(props);
                break;
            case 'checkbox':
                if (!config.blessedOn) break;
                blessedObj = blessed.checkbox(props);
                break;
            case 'radioset':
                if (!config.blessedOn) break;
                blessedObj = blessed.radioset(props);
                break;
            case 'radiobutton':
                if (!config.blessedOn) break;
                blessedObj = blessed.radiobutton(props);
                break;
            case 'prompt':
                if (!config.blessedOn) break;
                blessedObj = blessed.prompt(props);
                break;
            case 'question':
                if (!config.blessedOn) break;
                blessedObj = blessed.question(props);
                break;
            case 'message':
                if (!config.blessedOn) break;
                blessedObj = blessed.message(props);
                break;
            case 'loading':
                if (!config.blessedOn) break;
                blessedObj = blessed.loading(props);
                break;
            case 'progressbar':
                if (!config.blessedOn) break;
                blessedObj = blessed.progressbar(props);
                break;
            case 'log':
                if (!config.blessedOn) break;
                blessedObj = blessed.log(props);
                break;
            case 'table':
                if (!config.blessedOn) break;
                blessedObj = blessed.table(props);
                break;
            case 'terminal':
                if (!config.blessedOn) break;
                blessedObj = blessed.terminal(props);
                break;
            case 'image':
                if (!config.blessedOn) break;
                blessedObj = blessed.image(props);
                break;
            case 'ansiimage':
                if (!config.blessedOn) break;
                blessedObj = blessed.ansiimage(props);
                break;
            case 'overlayimage':
                if (!config.blessedOn) break;
                blessedObj = blessed.overlayimage(props);
                break;
            case 'video':
                if (!config.blessedOn) break;
                blessedObj = blessed.video(props);
                break;
            case 'layout':
                if (!config.blessedOn) break;
                blessedObj = blessed.layout(props);
                break;

            default:
              ___log_bl(`Unknown node kind ${node.nodeSubj}.`);
        }

        registry[node.nodeId] = { source : node, blessed : blessedObj };
        ___log_bl('registered node & blessed obj at', node.nodeId);

        node.handlers.forEach((handler) => {
            bindHandler(blessedObj, handler);
        });

        node.children.forEach((child) => {
            const childBlessed = registerNode(child)();
            if (blessedObj && childBlessed) blessedObj.append(childBlessed);
        });

        if (handlers[INIT_HANDLER_KEY]) {
            performInit(handlers[INIT_HANDLER_KEY]);
        }

        return blessedObj;

    }
}

function callCommand(rawNodeKey) {
    return function(command) {
        // ___log_bl('build', nodeId, command);
        return function() {
            const nodeId = rawNodeKey;
            ___log_bl('call', nodeId, command);
            let returnObj = null;

            if (config.blessedOn) {
                if (command.type === 'process') {
                    process[command.method].apply(this, command.args);
                } else {
                    const blessedObj = registry[nodeId] ? registry[nodeId].blessed : null;
                    let path, pathLen;

                    //console.log(blessedObj, blessedObj['selected']);
                    if (blessedObj) {
                        switch (command.type) {
                            case 'call':
                                if (command.marker == 'CallCommandEx') {
                                    ___log_bl('ex', command.method, command.args);
                                    if (command.method == 'setItems') {
                                        blessedObj['setItems'].apply(blessedObj, [ adaptListBarCommands(command.args) ]);
                                    } else if (command.method == 'addItemH') {
                                        blessedObj['addItem'].apply(blessedObj, adaptListBarSingleCommand(command.args));
                                    } else {
                                        blessedObj[command.method].apply(blessedObj, checkForNodes(command.args));
                                    }
                                } else {
                                    blessedObj[command.method].apply(blessedObj, command.args);
                                }
                                break;
                            case 'get':
                                returnObj = blessedObj[command.property];
                                break;
                            case 'getp':
                                path = command.path;
                                console.log(path, blessedObj);
                                if (path) {
                                    returnObj = path.reduce((trg, pitem) => trg[pitem], blessedObj);
                                }
                                break;
                            case 'set':
                                blessedObj[command.property] = command.value;
                                break;
                            case 'setp':
                                path = command.path;
                                pathLen = path.length;
                                if (path) {
                                    path.reduce((trg, pitem, idx) => {
                                        if (idx == (pathLen - 1)) {
                                            trg[pitem] = command.value;
                                            return null;
                                        } else {
                                            return trg[pitem];
                                        }
                                    }, blessedObj);
                                }
                                break;
                            case 'process':
                                // handled earlier
                                break;
                            case 'sub':
                                // handled earlier
                                break;
                            default:
                                break;
                        }

                    }

                }
            }

            return returnObj;
        }
    }
}

function checkForNodes(cmdArgs) {
    return cmdArgs.map((arg) => {
        if (arg && arg['marker'] && arg['marker'] == 'Node') {
            const node = arg;
            const blessedNode = registry[node.nodeId] ? registry[node.nodeId].blessed : registerNode(node)();
            // TODO: check registry[node.nodeId].blessed?
            return blessedNode;
        } else {
            return arg;
        }
    });
}

function callCommandEx(rawNodeKey) {
    return function(command) {
        return function(handlers) {

            // ___log_bl('ccex', 'command', command);
            ___log_bl('ccex', 'handlers', handlers);

            handlers.forEach((handler) => {
                handlersFns[handler.index] = handler;
            });

            return function() {
                const commandResult = callCommand(rawNodeKey)(command)();
                handlers.forEach((handler) => {
                    const blessedObj = registry[handler.nodeId] ? registry[handler.nodeId].blessed : null;
                    ___log_bl(handler.nodeId, handler.event, handler.index, blessedObj ? 'blessedObj found' : 'blessedObj not found');
                    // FIXME: for links, blessed is not found, also node ID is improper for the newly created nodes, but ok for newly created links
                    // ___log_bl('ccex', 'bindHandler before', handler.nodeId, registry[handler.nodeId], handlersFns[handler.index]);
                    if (blessedObj) {
                        // ___log_bl('ccex', 'bindHandler after', handler.nodeId, handler);
                        bindHandler(blessedObj, handler);
                    }
                });
                return commandResult;
            }
        }
    }
}

function adaptListBarCommands(lbItems) {
    const cmdsData = lbItems[0].map((v) => v[1]); // the hack to get rid of `jsonCmd` label that is added to make Argonaut properly encode
    const handlersRefs = lbItems.slice(1);
    const handlersRefsRec = buildRecord(handlersRefs, (h) => ({ name : h.eventUniqueId, value : h }));
    // ___log_bl(cmdsData, handlersRefsRec);
    if (cmdsData.length != handlersRefs.length) {
        ___log_bl('list bar commands and refs are unueqal in size');
        return [];
    }
    const commands = buildRecord(cmdsData, (cmd, index) => {
        return adaptListBarCommandValue(handlersRefsRec)(cmd, index)
    });

    return commands;
}

function adaptListBarSingleCommand(lbItems) {
    const cmds = adaptListBarCommands(lbItems);
    const cmdKey = Object.keys(cmds)[0];
    const singleCmd = cmds[cmdKey];
    return [ { text : cmdKey, callback : singleCmd.callback, keys : singleCmd.keys || [] } ];

}

function adaptListBarCommandValue(hs) {
    return function(cmd, idx) {
        ___log_bl('command', cmd);
        const localHandlerId = cmd.eventUID;
        const handlerRef = hs[localHandlerId];
        if (handlerRef) {
            const handlerIndex = handlerRef.index;
            const handlerFn = handlersFns[handlerIndex];
            if (handlerFn) {
                ___log_bl('command handler', localHandlerId, handlerIndex, handlerFn);
                return { name : cmd.command, value : { keys : cmd.keys, callback : (evt) => handlerFn.call(evt)() } };
            } else {
                ___log_bl('handler at', handlerIndex, ' not found');
                return { name : cmd.command, value : { keys : cmd.keys, callback : () => {} }};
            }
        } else {
            ___log_bl('local handler ', localHandlerId, ' not found');
        }
    }
}

function configureBlessedAndLogging(cfg) {
    return function() {
        config.blessedOn = cfg.blessedOn;
        config.loggingBlessedTo  = cfg.loggingBlessedTo;
        config.loggingCommandsTo = cfg.loggingCommandsTo;
    }
}

function logCommandWhenEnabled(fn) {
    return function() {
        ___log_cmd(fn);
    }
}

function ___log_bl() {
    if (config.loggingBlessedTo && config.loggingBlessedTo === 'console') {;
        console.log.apply(this, arguments);
    } else if (config.loggingBlessedTo) {
        if (!blessedLogFile) {
            blessedLogFile = fs.createWriteStream(config.loggingBlessedTo, { flags: 'a' });
        }
        blessedLogFile.write(util.format.apply(null, arguments) + '\n');
    }
}

function ___log_cmd(fn) {
    if (config.loggingCommandsTo && config.loggingCommandsTo === 'console') {;
        console.log(fn(null));
    } else if (config.loggingCommandsTo) {
        if (!commandsLogFile) {
            commandsLogFile = fs.createWriteStream(config.loggingCommandsTo, { flags: 'a' });
        }
        commandsLogFile.write(util.format(fn(null)) + '\n');
    }
}

export const execute_ = execute;
export const registerNode_ = registerNode;
export const callCommandEx_ = callCommandEx;
export const configureBlessedAndLogging_ = configureBlessedAndLogging;
export const logCommandWhenEnabled_ = logCommandWhenEnabled;