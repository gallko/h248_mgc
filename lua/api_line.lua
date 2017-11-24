---
--- Created by rus.
--- DateTime: 15.11.17 13:33
---
--- Don't change
---

local api_line = {}
local map_func = {
    sendRestoreService  = 1,
    subtractAll         = 2,
    subtract            = 3,

    sendModify          = 4,

    wait_events         = 5
}
local map_error = {
    [0]     = 'Successes',
    [1]     = 'Bad arguments',
    [2]     = 'Timeout of replay',


    [100]   = 'Binary data is wrong'
}
map_event = {
    ['timeout'] = 1,
    ['of'] = 2,
    ['on'] = 3,
    ['fl'] = 4,
    ['digit'] = 5,
    ['number'] = 6,
    ['disconnect_remote'] = 7
}
map_streamMode = {
    sendOnly    = 'sendOnly',
    recvOnly    = 'recvOnly',
    sendRecv    = 'sendRecv',
    inactive    = 'inactive',
    loopBack    = 'loopBack'
}

function api_line:new(termID, context)
    newObj = {
        _termID = termID,
        _context = context,
        _error = 0
    }
    self.__index = self
    return setmetatable(newObj, self)
end

function api_line:printall()
    if self._termID ~= nil then
        print("TID: " .. self._termID)
    end
    if self._context ~= nil then
        print("Ctx: " .. self._context)
    end
    if self._signal ~= nil then
        print("Signal: " .. self._signal)
    end
    if self._eventID ~= nil then
        print("EventID: " .. self._eventID)
    end
    if self._error ~= nil then
        print("Error: " .. self._error)
    end
end

function api_line:get_error()
    return self._error, map_error[self._error]
end

function api_line:sendRestoreService()
    local result = erlCallbackFunc(map_func.sendRestoreService, self._termID)
    self._error = result
    return (result == 0)
end
function api_line:subtractAll()
    -- delete TermID in all context
    local result = erlCallbackFunc(map_func.subtractAll, self._termID)
    self._error = result
    return (result == 0)
end
function api_line:subtract()
    -- delete TermID in current context
    local result
    if self._context == 0 then
        result = erlCallbackFunc(map_func.subtractAll, self._termID)
    else
        result = erlCallbackFunc(map_func.subtract, self._context, self._termID)
    end
    self._error = result
    return result == 0
end

function api_line:setSignal(strSignal)
    self._signal = strSignal
end

function api_line:setEvents(idEvents)
    self._event = idEvents
end

function api_line:setStreamMode(streamMode)
    self._streamMode = streamMode
end

function api_line:sendModify()
--[Ctx, TermID, Events, Signal, StreamMode, ReserveValue, ReserveGroup, tdmc_EchoCancel, tdmc_Gain]
    local result = erlCallbackFunc(map_func.sendModify,
    self._context,
    self._termID,
    self._event,
    self._signal,
    self._streamMode,
    self._reserveValue,
    self._reserveGroup,
    self._tdmc_EchoCancel,
    self._tdmc_Gain)
    if result == 0 then
        -- delete the applied parameters
        self._event = nil
        self._signal = nil
        self._streamMode = nil
        self._reserveValue = nil
        self._reserveGroup = nil
        self._tdmc_EchoCancel = nil
        self._tdmc_Gain = nil
    end
    self._error = result
    return result == 0
end

function api_line:wait_events(timeout)
    local result, event = erlCallbackFunc(map_func.wait_events, timeout)
    return event
end

return api_line