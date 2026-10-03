type INotifications<'a, 'b, 'c, 'd, 'e> = class end

type DeviceNotificationHandler<'Notification, 'CallbackId, 'RegisterInputData, 'RegisterOutputData, 'UnregisterOutputData>
    private
    (
        client:
            INotifications<'Notification, 'CallbackId, 'RegisterInputData, 'RegisterOutputData, 'UnregisterOutputData>,
        callbackId: 'CallbackId,
        validateUnregisterOutputData: 'UnregisterOutputData -> unit
    ) =
    let a = 5
