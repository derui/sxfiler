import assert from 'power-assert';
import MainIPC from 'sxfiler/main/MainIPC';
import {IPCKeys} from 'sxfiler/common/Constants';
import Stub from './IpcStub';
import sinon from 'sinon';
import fs from 'fs';
import path from 'path';
import tmp from 'tmp';

/** @test {MainIPC} */
describe('MainIPC', () => {
  let obj = null;
  before(() => {
    obj = tmp.dirSync();
    fs.writeFileSync(path.join(obj.name, 'file.txt'), 'data');
  });

  after(() => {
    fs.unlinkSync(path.join(obj.name, 'file.txt'));
    obj.removeCallback();
  });

  it('should be able to instance', () => {
    assert.ok(new MainIPC(Stub(), fs) instanceof MainIPC);
  });

  it('can handle request to get file informations', (cb) => {
    let stub = Stub();
    let ipc = new MainIPC(stub, fs);
    stub.on(IPCKeys.FINISH_FILES_IN_DIRECTORY, ([err, files]) => {
      assert.equal(err, null);
      assert.ok(!!files['file.txt']);
      cb();
    });

    stub.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, obj.name);
  });

  it('should return error when request path is no anywhere', (cb) => {
    let stub = Stub();
    let ipc = new MainIPC(stub, fs);
    stub.on(IPCKeys.FINISH_FILES_IN_DIRECTORY, ([err, files]) => {
      assert.notEqual(err, null);
      assert.deepEqual(files, {});
      cb();
    });

    stub.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, 'notfound');
  });

  it('should return error when path is file', (cb) => {
    let stub = Stub();
    let ipc = new MainIPC(stub, fs);
    stub.on(IPCKeys.FINISH_FILES_IN_DIRECTORY, ([err, files]) => {
      assert.notEqual(err, null);
      assert.deepEqual(files, {});
      cb();
    });

    stub.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, path.join(obj.name, 'file.txt'));
  });
});
