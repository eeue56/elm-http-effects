import flask

from flask import Flask, jsonify
app = Flask(__name__)

@app.route('/users')
def users():
    return jsonify(["dave"])

@app.route('/')
def root():
	print("hello")
	return app.send_static_file("index.html")

def main():
	app.run()

if __name__ == '__main__':
	main()