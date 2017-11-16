local m = require('lua.api_line')

function registration(termID, bin)
    Tid = m:new(termID, bin)
    R = Tid:sendRestoreService()
    S = Tid:subtract()
    Tid:sendModify()
    return true
end

function test(termID, ctx, signal, event)
    Tid = m:new(termID, ctx, signal, event)
    Tid:printall()
    --return 0
end