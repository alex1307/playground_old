#!/bin/bash
/Users/alex/Software/workbench/wrk -t12 -c400 -d30s -s login_post_message.lua https://localhost:8043/protected/message