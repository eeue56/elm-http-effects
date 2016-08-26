import flask

from flask import Flask, jsonify
import random

app = Flask(__name__)

@app.route('/dave')
def dave():
    return jsonify("dave")

@app.route('/users')
def users():
	random_name = random.choice(["rabbit", "cat", "dog", "fish", "goat", "boat", "banana", "sausage"])
	return jsonify([random_name])



@app.route('/')
def root():
	print("hello")
	return app.send_static_file("index.html")

def main():
	app.run()

if __name__ == '__main__':
	main()