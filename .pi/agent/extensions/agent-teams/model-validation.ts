export interface ModelReference {
  provider: string;
  id: string;
  name?: string;
}

export interface ModelRegistryLike {
  getAll?: () => ModelReference[];
  getAvailable?: () => ModelReference[];
}

export interface ResolvedTeammateModel {
  provider: string;
  id: string;
  /** provider/id — the registry-canonical model name, no thinking suffix. */
  canonical: string;
  /** provider/id[:thinking] — the full string to pass to `pi --model`. */
  cliModel: string;
  /** Pi thinking level after the canonical id, if specified (e.g. "xhigh"). */
  thinking?: string;
  /** The exact string the caller requested (pre-resolution). */
  requested: string;
  model: ModelReference;
}

export type TeammateModelValidationResult =
  | { ok: true; model: ResolvedTeammateModel | undefined }
  | { ok: false; text: string };

// Pi documents these thinking levels as suffixes on the model id (provider/id:level).
// Mirror that list here so bare-id resolution can preserve a suffix while still
// validating the underlying model against the registry.
const THINKING_LEVELS = new Set(["off", "minimal", "low", "medium", "high", "xhigh", "max"]);

function canonicalModelName(model: ModelReference): string {
  return `${model.provider}/${model.id}`;
}

function getRegisteredModels(modelRegistry: ModelRegistryLike | undefined): ModelReference[] {
  // Use getAll() when available to mirror Pi's CLI model lookup: validate that
  // the model name is registered, without turning missing credentials into a
  // spawn-time validation error.
  if (typeof modelRegistry?.getAll === "function") return modelRegistry.getAll();
  if (typeof modelRegistry?.getAvailable === "function") return modelRegistry.getAvailable();
  return [];
}

function resolveModel(
  model: ModelReference,
  requested: string,
  thinking: string | undefined,
): ResolvedTeammateModel {
  const canonical = canonicalModelName(model);
  return {
    provider: model.provider,
    id: model.id,
    canonical,
    cliModel: thinking ? `${canonical}:${thinking}` : canonical,
    ...(thinking ? { thinking } : {}),
    requested,
    model,
  };
}

function formatAmbiguousModelError(requested: string, matches: ModelReference[]): string {
  const candidates = Array.from(new Set(matches.map(canonicalModelName))).sort();
  return [
    `Ambiguous model "${requested}" matches multiple providers:`,
    ...candidates.map((candidate) => `- ${candidate}`),
    "",
    "Use one of the provider/model values above, or omit model to inherit the lead model.",
  ].join("\n");
}

function formatUnknownModelError(requested: string, hasRegisteredModels: boolean): string {
  const hint = hasRegisteredModels
    ? "Use a valid provider/model from `pi --list-models`, or omit model to inherit the lead model."
    : "No models are registered in Pi's model registry. Omit model to inherit the lead model.";
  return [`Unknown model "${requested}".`, hint].join("\n");
}

/**
 * Try to match `value` against the registry as either a canonical
 * provider/id reference or a unique bare id. Returns the matching model on
 * success, or an error string for ambiguity. Returns null when nothing matched.
 */
function findRegistryMatch(
  value: string,
  models: ModelReference[],
): { model: ModelReference } | { ambiguous: ModelReference[] } | null {
  const normalized = value.toLowerCase();

  const canonicalMatches = models.filter(
    (model) => canonicalModelName(model).toLowerCase() === normalized,
  );
  if (canonicalMatches.length > 0) return { model: canonicalMatches[0] };

  const idMatches = models.filter((model) => model.id.toLowerCase() === normalized);
  if (idMatches.length === 1) return { model: idMatches[0] };
  if (idMatches.length > 1) return { ambiguous: idMatches };

  return null;
}

export function validateTeammateModel(
  requestedModel: string | undefined,
  modelRegistry: ModelRegistryLike | undefined,
): TeammateModelValidationResult {
  if (requestedModel === undefined) return { ok: true, model: undefined };

  const requested = requestedModel.trim();
  if (!requested) {
    return {
      ok: false,
      text: "Model reference is empty. Omit model to inherit the lead model, or pass a valid provider/model.",
    };
  }

  const models = getRegisteredModels(modelRegistry);

  // First try the requested string verbatim — this honours model ids that
  // legitimately contain colons and avoids silently stripping any suffix.
  const directMatch = findRegistryMatch(requested, models);
  if (directMatch && "model" in directMatch) {
    return { ok: true, model: resolveModel(directMatch.model, requested, undefined) };
  }
  if (directMatch && "ambiguous" in directMatch) {
    return { ok: false, text: formatAmbiguousModelError(requested, directMatch.ambiguous) };
  }

  // Fall back to thinking-suffix handling: only strip a trailing `:level` if
  // it is a documented Pi thinking level. Unknown suffixes stay attached so
  // they fail with a clear unknown-model error rather than silently resolving
  // to a different model.
  const lastColon = requested.lastIndexOf(":");
  if (lastColon > 0) {
    const suffix = requested.slice(lastColon + 1);
    const prefix = requested.slice(0, lastColon);
    if (THINKING_LEVELS.has(suffix.toLowerCase()) && prefix.length > 0) {
      const prefixMatch = findRegistryMatch(prefix, models);
      if (prefixMatch && "model" in prefixMatch) {
        return {
          ok: true,
          model: resolveModel(prefixMatch.model, requested, suffix.toLowerCase()),
        };
      }
      if (prefixMatch && "ambiguous" in prefixMatch) {
        return { ok: false, text: formatAmbiguousModelError(requested, prefixMatch.ambiguous) };
      }
    }
  }

  return { ok: false, text: formatUnknownModelError(requested, models.length > 0) };
}
