import * as fs from "node:fs";
import * as path from "node:path";

export function resolveSpawnCwd(requestedCwd: string | undefined, defaultCwd: string, callerCwd: string): string {
	if (!requestedCwd) return defaultCwd;

	const candidate = path.isAbsolute(requestedCwd)
		? requestedCwd
		: path.resolve(callerCwd, requestedCwd);

	try {
		const stat = fs.statSync(candidate);
		if (!stat.isDirectory()) {
			throw new Error("not a directory");
		}
		return fs.realpathSync(candidate);
	} catch {
		throw new Error(`Spawn cwd does not exist or is not a directory: ${candidate}`);
	}
}
