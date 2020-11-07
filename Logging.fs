module CodeChallenge.Logging
    
open Serilog.Events
open Serilog.Formatting.Json
open System.IO
open System
    
type VeepeeJsonFormatter() =
    inherit JsonFormatter()
    
    let getIntLevelValue (level:LogEventLevel) =
        match level with
        | LogEventLevel.Verbose -> 7
        | LogEventLevel.Debug -> 7
        | LogEventLevel.Information -> 6
        | LogEventLevel.Warning -> 4
        | LogEventLevel.Error -> 3
        | LogEventLevel.Fatal -> 1
        | _ -> 1
    
    member private this.WriteSingleException(exn:exn, delim:byref<string>, output:TextWriter,depth:int) =
        this.WriteJsonProperty("message", exn.Message, &delim, output)
        this.WriteJsonProperty("source", exn.Source, &delim, output)
        this.WriteJsonProperty("stackTrace", exn.StackTrace, &delim, output)

    member private this.WriteExceptionTree(exn:exn, delim:byref<string>, output:TextWriter,depth:int) =
        delim <- ""
        this.WriteSingleException(exn, &delim, output, depth)
        let innerExn = exn.InnerException
        if not (isNull innerExn)
        then
            output.Write(",")
            output.Write("\"innerException\":{")
            this.WriteExceptionTree(exn, &delim, output, depth + 1)
            output.Write("}")
         
    override x.WriteProperties(properties, output) =
        output.Write(",")
        x.WritePropertiesValues(properties, output)
    
    override x.WriteMessageTemplate(template, delim, output) = x.WriteJsonProperty("short_message", template, &delim, output)

    override x.WriteTimestamp(timestamp, delim, output) = x.WriteJsonProperty("timestamp", timestamp.ToUnixTimeSeconds(), &delim, output)

    override x.WriteLevel( level, delim, output) =
        x.WriteJsonProperty("level_as_string", level, &delim, output)
        x.WriteJsonProperty("level", getIntLevelValue(level), &delim, output)

    override x.WriteException(error, delim, output) =
        output.Write(delim)
        output.Write("\"");
        output.Write("exception");
        output.Write("\":{");
        x.WriteExceptionTree(error, ref delim, output, 0);
        output.Write("}");


    override  x.WriteJsonProperty( name,  value, precedingDelimiter, output) =
       
        match value with
        | :? ScalarValue as v when isNull v.Value -> ignore()
        | _ -> 
            let name = String.Concat(Char.ToLower(name.[0]), name.Substring(1))
            base.WriteJsonProperty(name, value, &precedingDelimiter, output)
