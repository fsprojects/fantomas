using System.Text.Json;
using System.Threading.Tasks;
using PolyType;
using StreamJsonRpc;
using static Fantomas.Client.Contracts;
using static Fantomas.Client.LSPFantomasServiceTypes;

namespace Fantomas.Daemon.Contract;

/// <summary>
/// The methods the daemon answers. <c>FantomasDaemon</c> implements this, and StreamJsonRpc
/// dispatches to it through the shape generated for it rather than by reflection.
/// </summary>
/// <remarks>
/// Its shape is generated on <see cref="DaemonShapes"/> rather than on the interface itself.
/// <c>[GenerateShape]</c> here would add a static abstract member for the implementation to satisfy,
/// which F# cannot.
/// </remarks>
[TypeShape(IncludeMethods = MethodShapeFlags.PublicInstance)]
public interface IFantomasDaemon
{
    /// <summary>The version of Fantomas behind this daemon.</summary>
    [JsonRpcMethod(Methods.Version)]
    string Version();

    /// <summary>Every setting Fantomas reads from an <c>.editorconfig</c>, as a JSON document.</summary>
    [JsonRpcMethod(Methods.Configuration)]
    string Configuration();

    /// <summary>Format a whole document.</summary>
    [JsonRpcMethod(Methods.FormatDocument, UseSingleObjectParameterDeserialization = true)]
    Task<FormatDocumentResponse> FormatDocumentAsync(FormatDocumentRequest request);

    /// <summary>Format a selection within a document.</summary>
    [JsonRpcMethod(Methods.FormatSelection, UseSingleObjectParameterDeserialization = true)]
    Task<FormatSelectionResponse> FormatSelectionAsync(FormatSelectionRequest request);
}

/// <summary>Where PolyType generates the shape of <see cref="IFantomasDaemon"/>.</summary>
[GenerateShapeFor<IFantomasDaemon>]
public partial class DaemonShapes;

/// <summary>What the daemon hands to StreamJsonRpc, built from the generated shape.</summary>
public static class DaemonContract
{
    /// <summary>The daemon's methods, described without reflection.</summary>
    public static RpcTargetMetadata Metadata { get; } = RpcTargetMetadata.FromShape<IFantomasDaemon, DaemonShapes>();

    /// <summary>
    /// A formatter that writes the envelope itself and leaves what goes inside it, the requests and
    /// responses, to <paramref name="userData"/>.
    /// </summary>
    /// <remarks>
    /// StreamJsonRpc marks <see cref="PolyTypeJsonFormatter"/> as experimental. It is the formatter it
    /// documents as Native AOT ready, and the wire tests hold it to what the daemon has always sent.
    /// </remarks>
    public static IJsonRpcMessageFormatter CreateFormatter(JsonSerializerOptions userData)
    {
#pragma warning disable PolyTypeJson
        return new PolyTypeJsonFormatter
        {
            TypeShapeProvider = PolyType.SourceGenerator.TypeShapeProvider_Fantomas_Daemon_Contract.Default,
            JsonSerializerOptions = userData,
        };
#pragma warning restore PolyTypeJson
    }
}
