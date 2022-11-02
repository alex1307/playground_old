-- example HTTP POST script which demonstrates setting the
-- HTTP method, body, and adding a header

wrk.method = "POST"
wrk.body   = '{"uuid": "b7cc9b97-d242-4439-b61a-363d6dd7d488",\
	"from": {\
		"first_name": "Alexander",\
		"last_name": "Popov",\
		"email": "off@eclear.com"\
	},\
	"created_at": 1619021027485,\
	"subject": "Subject",\
	"body": "what a body....",\
	"recipients": [{\
		"first_name": "Oliver",\
		"last_name": "Port",\
		"email": "007@www.com"\
	}],\
	"attachments": [{\
		"message_id": "b7cc9b97-d242-4439-b61a-363d6dd7d488",\
		"name": "dont.download.me",\
		"file_type": "zip",\
		"payload": [88, 99, 110, 107]\
	}]\
}'
wrk.headers["Content-Type"] = "application/json"