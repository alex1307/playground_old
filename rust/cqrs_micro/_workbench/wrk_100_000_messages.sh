#!/bin/bash
/Users/alex/Software/workbench/wrk -t12 -c400 -d30s -s post_message.lua http://localhost:8044/users/message