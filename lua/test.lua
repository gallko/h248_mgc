local m = require('lua.api_line')

function registration(termID, context, bin)
    local Tid = m:new(termID, context, bin)
    local R = Tid:sendRestoreService()
    local S = Tid:subtract()
    Tid:setSignal('null')
    Tid:setEvents(1)
    Tid:setStreamMode(map_streamMode.inactive)
    Tid:sendModify()
    return true
end

function start_talk(termID, context, bin)
    local Tid = m:new(termID, context, bin)
    Tid:setSignal('cg/dt')
    Tid:setEvents(1)
    Tid:printall()
    Tid:sendModify()
    return true
end

function test(termID, ctx, signal, event)
    local Tid = m:new(termID, ctx, signal, event)
    Tid:printall()
    --return 0
end