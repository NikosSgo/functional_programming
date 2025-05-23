module MailBoxProcessorAgent

type AgentMessage =
    | Print of string
    | Add of int
    | GetTotal of AsyncReplyChannel<int>
    | Stop

type LoggerAgent() =
    let agent = MailboxProcessor<AgentMessage>.Start(fun inbox ->
        let rec loop total =
            async {
                let! msg = inbox.Receive()
                match msg with
                | Print text ->
                    printfn "[LOG] %s" text
                    return! loop total
                | Add x ->
                    printfn "[ADD] Прибавляю %d" x
                    return! loop (total + x)
                | GetTotal reply ->
                    reply.Reply total
                    return! loop total
                | Stop ->
                    printfn "[STOP] Агент завершает работу. Сумма: %d" total
                    return ()
            }
        loop 0
    )

    member _.Post(msg) = agent.Post(msg)
    member _.PostAndReply(msgBuilder) = agent.PostAndReply(msgBuilder)


let agentMain =
    let logger = LoggerAgent()

    logger.Post(Print "\nАгент запущен")
    logger.Post(Add 10)
    logger.Post(Add 5)
    logger.Post(Print "Прибавили 10 и 5")

    let result = logger.PostAndReply(fun reply -> GetTotal reply)
    printfn "Общая сумма: %d" result

    logger.Post(Stop)


