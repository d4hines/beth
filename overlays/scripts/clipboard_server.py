#!/usr/bin/env nix-shell
#!nix-shell -i python -p python3 python3Packages.flask terminal-notifier

from flask import Flask, request, jsonify
import subprocess
import json
import os

app = Flask(__name__)

@app.route('/command-done', methods=['POST'])
def command_done():
    data = request.json
    cmd = data.get('cmd')
    ftime = data.get('ftime')
    
    subprocess.run(['terminal-notifier', '-title', f'Done: {cmd}', '-message', f'Time: {ftime}'])
    subprocess.run(['afplay', os.path.expanduser('~/done.wav')])
    
    return jsonify({"status": "success"}), 200

@app.route('/put-clipboard', methods=['POST'])
def put_clipboard():
    message = request.get_data(as_text=True)
    result = subprocess.run(['pbcopy'], input=message, capture_output=True, text=True)
    if result.returncode == 0:
        return jsonify({"status": "success", "message": message}), 200
    else:
        return jsonify({"status": "error", "message": result.stderr}), 500

@app.route('/get-clipboard', methods=['GET'])
def get_clipboard():
    result = subprocess.run(['pbpaste'], capture_output=True, text=True)
    if result.returncode == 0:
        return jsonify({"status": "success", "content": result.stdout}), 200
    else:
        return jsonify({"status": "error", "message": result.stderr}), 500

@app.route('/open-vscode-remote', methods=['POST'])
def open_vscode_remote():
    data = request.json
    path = data.get('path')
    
    if not path:
        return jsonify({"status": "error", "message": "Path is required"}), 400
    
    command = ['code', '--remote', 'ssh-remote+nixos.local', path]
    
    try:
        result = subprocess.run(command, capture_output=True, text=True, check=True)
        return jsonify({"status": "success", "message": "VSCode remote opened successfully"}), 200
    except subprocess.CalledProcessError as e:
        return jsonify({"status": "error", "message": f"Failed to open VSCode remote: {e.stderr}"}), 500

if __name__ == '__main__':
    port = 49153
    print(f"Starting server on port {port}")
    app.run(port=port)
