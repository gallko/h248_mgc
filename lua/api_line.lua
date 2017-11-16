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

    sendModify          = 4
}
local map_error = {
    [0]     = 'Successes',
    [1]     = 'Bad arguments',
    [2]     = 'Timeout of replay',


    [100]   = 'Binary data is wrong'
}
local map_event = {
    [0]     = 'ok',
    [1]     = 'timeout',
    [2]     = 'off_hook',
    [3]     = 'on_hook',
    [4]     = 'flash',
    [5]     = 'digit',
    [6]     = 'number',
    [7]     = 'disconnect_remote'
}

function api_line:new(termID, bin)
    newObj = {
        _bin = bin,
        _termID = termID,
        _error = 0
    }
    self.__index = self
    return setmetatable(newObj, self)
end
function api_line:printall()
    print("TID: " .. self._termID)
    print("Ctx: " .. self._context)
    print("Signal: " .. self._signal)
    print("EventID: " .. self._eventID)
    print("Error: " .. self._error)
end
function api_line:get_error()
    return self._error, map_error[self._error]
end
function api_line:sendRestoreService()
    local result = erlCallbackFunc(map_func.sendRestoreService, self._bin, self._termID)
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
    if self._context == nil then
        result = erlCallbackFunc(map_func.subtractAll, self._bin, self._termID)
    else
        result = erlCallbackFunc(map_func.subtract, self._bin, self._context, self._termID)
    end
    self._error = result
    return result == 0
end

function api_line:setSignal(strSignal)
    self._signal = strSignal
end

function api_line:setEvents(idEvents)
    self._events = idEvents
end


function api_line:sendModify()
--[Ctx, TermID, Events, Signal, StreamMode, ReserveValue, ReserveGroup, tdmc_EchoCancel, tdmc_Gain]
    local result = erlCallbackFunc(map_func.sendModify, self._bin,
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
        self._context = nil
        self._termID = nil
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

return api_line