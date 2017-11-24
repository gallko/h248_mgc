local m = require('lua.api_line')

function registration(termID, context)
    local Tid = m:new(termID, context)
    local R = Tid:sendRestoreService()
    local S = Tid:subtract()
    Tid:setSignal('null')
    Tid:setEvents(1)
    Tid:setStreamMode(map_streamMode.inactive)
    Tid:sendModify()
    return true
end

function start_talk(termID, context)
    local Tid = m:new(termID, context)
    Tid:setSignal('cg/dt')
    Tid:setEvents(2)
    Tid:sendModify()

    local event
    repeat
        event = Tid:wait_events(20)
        print("Returned event: " .. event)
        if event == 'of' then
            Tid:setSignal('cg/bt')
            Tid:sendModify()
        elseif event == 'timeout' then
            Tid:setSignal('cg/bt')
            Tid:setEvents(2)
            Tid:sendModify()
        end
    until event == 'on'
    Tid:setSignal('null')
    Tid:setEvents(1)
    Tid:sendModify()
    return true
end

function test(termID, ctx, signal, event)
    local Tid = m:new(termID, ctx, signal, event)
    Tid:printall()
    --return 0
end