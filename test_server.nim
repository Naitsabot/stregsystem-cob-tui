import asynchttpserver, asyncdispatch, json, strutils, times

proc handleRequest(req: Request) {.async.} =
    echo "=== Incoming Request ==="
    echo "Method: ", req.reqMethod
    echo "URL: ", req.url.path
    echo "Headers:"
    for key, val in req.headers:
        echo "  ", key, ": ", val
    echo "Body: ", req.body
    echo "========================"
    
    let path = req.url.path
    
    # Route handling
    if path == "/":
        await req.respond(Http200, "Hello from Nim test server!\n")
    
    elif path == "/echo":
        let response = %* {
            "method": $req.reqMethod,
            "path": path,
            "body": req.body,
            "timestamp": $now()
        }
        await req.respond(Http200, $response & "\n", 
            newHttpHeaders([("Content-Type", "application/json")]))
    
    elif path == "/test":
        await req.respond(Http200, "Test endpoint working!\n")
    
    elif req.reqMethod == HttpPost and path == "/api/purchase":
        # Simulate stregsystem purchase endpoint
        try:
            let jsonBody = parseJson(req.body)
            let username = jsonBody["username"].getStr()
            let item = jsonBody["item"].getStr()
            
            let response = %* {
                "status": "success",
                "message": "Purchase recorded",
                "username": username,
                "item": item,
                "timestamp": $now()
            }
            
            await req.respond(Http200, $response & "\n",
                newHttpHeaders([("Content-Type", "application/json")]))
        except:
            await req.respond(Http400, "Invalid JSON\n")
    
    else:
        await req.respond(Http404, "Not found\n")

proc main() {.async.} =
    let server = newAsyncHttpServer()
    echo "Starting HTTP server on localhost:8080"
    echo "Available endpoints:"
    echo "  GET  /"
    echo "  GET  /test"
    echo "  GET  /echo"
    echo "  POST /api/purchase (expects JSON with username and item)"
    echo ""
    echo "Press Ctrl+C to stop"
    echo ""
    
    server.listen(Port(8080))
    
    while true:
        if server.shouldAcceptRequest():
            await server.acceptRequest(handleRequest)
        else:
            await sleepAsync(10)

waitFor main()
