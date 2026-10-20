// Reports a minimal Fantomas sample the way the "Looks wrong? Create an issue!" button on
// https://fsprojects.github.io/fantomas-tools/#/fantomas/main does. The link, the body template and
// the version line mirror src/client/fsharp/FantomasOnline/View.fs in fantomas-tools; keep them in
// step with it when that changes.
//
// Runs on Bun, or on Node 22.18 and later, which strip the types themselves:
//
//   bun report.ts check <sample> [--fsi] [--setting Key=Value ...]
//   bun report.ts issue <sample> --title <title> [--description <file>] [--fsi]
//                       [--setting Key=Value ...] [--compiles] [--no-open]

// lz-string is CommonJS and assigns its exports at runtime, so Node only sees the default export.
import LZString from "lz-string";
import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { parseArgs } from "node:util";

// The main stage fantomas-tools itself calls, from `setViteToProduction` in its build.fsx.
const backend =
  "https://arlp8cgo97.execute-api.eu-west-1.amazonaws.com/fantomas-main-stage-1c52a6a/fantomas/main";
const toolsUrl = "https://fsprojects.github.io/fantomas-tools/#/fantomas/main";

// GitHub drops an issues/new link long before browsers do, and its limit covers the cookies too.
// Measured on 2026-09-23: without cookies, links up to about 7,060 characters went through; with a
// 2.5 KB cookie, only up to about 4,500. A logged-in session carries 1 to 3 KB of them, so stay well
// below: a body that does not fit still gets reported, through the clipboard.
const maxIssueUrlLength = 4000;

type Option = { $type: string; $value: [number, string, number | boolean | string] };
type Diagnostic = {
  range: { startLine: number; startColumn: number };
  severity: string;
  errorNumber: number;
  message: string;
};
type SourceComment = { range: { startLine: number }; text: string };
// What the backend found wrong with the result: `FormatIssue` in src/shared/FantomasOnlineShared.fs.
// Main only finds the first three; preview runs Fantomas 9's own checks and can find all of them.
type FormatIssue =
  | { kind: "notValidFSharp"; defines: string[]; diagnostics: Diagnostic[] }
  | { kind: "notIdempotent"; formattedAgain: string }
  | { kind: "checkFailed"; check: string; message: string }
  | { kind: "missingComment"; comment: SourceComment }
  | { kind: "commentsChanged"; defines: string[]; missing: SourceComment[]; added: string[] }
  | { kind: "directivesChanged"; defines: string[]; missing: string[]; added: string[] };
type FormatResponse = { code: string; issues: FormatIssue[] };
type Outcome =
  | { kind: "invalid-source"; message: string }
  | { kind: "error"; message: string }
  | { kind: "issues"; result: FormatResponse }
  | { kind: "formatted"; result: FormatResponse };

const { positionals, values } = parseArgs({
  allowPositionals: true,
  options: {
    fsi: { type: "boolean", default: false },
    setting: { type: "string", multiple: true, default: [] },
    title: { type: "string" },
    description: { type: "string" },
    compiles: { type: "boolean", default: false },
    "no-open": { type: "boolean", default: false },
  },
});

const [command, samplePath] = positionals;
if ((command !== "check" && command !== "issue") || !samplePath) {
  fail("usage: report.ts check|issue <sample> [--fsi] [--setting Key=Value] [--title ...]");
}

const code = readFileSync(samplePath, "utf8");
const [version, defaults] = await Promise.all([
  fetchText(`${backend}/version`),
  fetchText(`${backend}/options`).then((json) => JSON.parse(json) as Option[]),
]);
const changed = applySettings(defaults, values.setting);
const outcome = await format(code, values.fsi, defaults, changed);

if (command === "check") {
  printCheck(outcome);
} else {
  if (!values.title) fail("issue needs --title");
  if (outcome.kind === "invalid-source" && !values.compiles) {
    fail(
      `not reporting: the sample itself does not parse.\n${outcome.message}\n` +
        "If the real compiler accepts it, the parser Fantomas vendors is the bug: pass --compiles.",
    );
  }
  // Code the compiler accepts and Fantomas does not is reported with the parse error as the error.
  // fantomas-tools has no button for it, and blames the code instead.
  const reported = outcome.kind === "invalid-source" ? { kind: "error" as const, message: outcome.message } : outcome;
  const description = values.description ? readFileSync(values.description, "utf8").trim() : null;
  openIssue(values.title, buildBody(reported, description));
}

function fail(message: string): never {
  console.error(message);
  process.exit(1);
}

async function fetchText(url: string, init?: RequestInit): Promise<string> {
  const response = await fetch(url, init);
  const text = await response.text();
  if (!response.ok && response.status !== 400 && response.status !== 500) {
    fail(`${url} answered ${response.status}: ${text}`);
  }
  return text;
}

/** The options the user changed, with their new values. Unknown keys are an error, not ignored. */
function applySettings(defaults: Option[], settings: string[]): Option[] {
  return settings.map((setting) => {
    const [key, raw] = setting.split("=", 2);
    const option = defaults.find((o) => o.$value[1] === key);
    if (!option || raw === undefined) {
      fail(`unknown setting ${setting}, expected one of: ${defaults.map((o) => o.$value[1]).join(", ")}`);
    }
    const value = option.$type === "int" ? Number(raw) : option.$type === "bool" ? raw === "true" : raw;
    return { $type: option.$type, $value: [option.$value[0], key, value] };
  });
}

async function format(code: string, isFsi: boolean, defaults: Option[], changed: Option[]): Promise<Outcome> {
  const options = defaults.map((d) => changed.find((c) => c.$value[1] === d.$value[1]) ?? d);
  const response = await fetch(`${backend}/format`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ sourceCode: code, options, isFsi }),
  });
  const text = await response.text();
  if (!response.ok) {
    const error = tryParse(text);
    const invalidSource = error?.kind === undefined ? response.status === 400 : error.kind === "invalidSource";
    return { kind: invalidSource ? "invalid-source" : "error", message: error?.message ?? text };
  }
  const result = JSON.parse(text) as FormatResponse;
  return { kind: result.issues.length > 0 ? "issues" : "formatted", result };
}

function tryParse(text: string): { kind?: string; message?: string } | null {
  try {
    return JSON.parse(text);
  } catch {
    return null;
  }
}

function printCheck(outcome: Outcome) {
  console.log(`Fantomas ${version}`);
  console.log(`outcome: ${outcome.kind}\n`);
  switch (outcome.kind) {
    case "invalid-source":
    case "error":
      console.log(outcome.message);
      break;
    case "issues":
      for (const issue of outcome.result.issues) {
        const [heading, lines] = describeIssue(issue);
        console.log(`${issue.kind}: ${heading}`);
        for (const line of lines) console.log(`  ${line}`);
        if (issue.kind === "notValidFSharp") {
          for (const d of issue.diagnostics) {
            console.log(`  (${d.range.startLine},${d.range.startColumn}) FS${d.errorNumber}: ${d.message}`);
          }
        }
      }
      console.log(`\n--- result\n${outcome.result.code}`);
      for (const issue of outcome.result.issues) {
        if (issue.kind === "notIdempotent") console.log(`--- formatted again\n${issue.formattedAgain}`);
      }
      break;
    case "formatted":
      console.log(outcome.result.code);
      break;
  }
}

/** Where an issue was found, said only when there is something to say. */
function underDefines(defines: string[]): string {
  return defines.length === 0 ? "" : ` with ${defines.join(", ")} defined`;
}

/** An issue as a heading and the lines that explain it, worded as `describeIssue` in View.fs. */
function describeIssue(issue: FormatIssue): [string, string[]] {
  const atLine = (comment: SourceComment) => `"${comment.text}" at line ${comment.range.startLine}`;
  const changes = (missing: string[], added: string[]) => [
    ...missing.map((m) => `Lost: ${m}`),
    ...added.map((a) => `Added: "${a}"`),
  ];

  switch (issue.kind) {
    case "missingComment":
      return ["Fantomas lost a comment", [`The result does not have the comment ${atLine(issue.comment)} of your code.`]];
    case "notValidFSharp":
      return [
        "The result is not valid F#",
        [`Fantomas cannot parse the code it produced${underDefines(issue.defines)}. The diagnostics are about that code.`],
      ];
    case "commentsChanged":
      return [`The comments changed${underDefines(issue.defines)}`, changes(issue.missing.map(atLine), issue.added)];
    case "directivesChanged":
      return [
        `The directives changed${underDefines(issue.defines)}`,
        changes(
          issue.missing.map((m) => `"${m}"`),
          issue.added,
        ),
      ];
    case "notIdempotent":
      return [
        "The result was not idempotent",
        ["Formatting the result again changed it. The result after the first format is shown."],
      ];
    case "checkFailed":
      if (issue.check === "Idempotency") return ["Formatting the result again failed", [issue.message]];
      if (issue.check === "TriviaComparison") return ["Reading the comments of the result failed", [issue.message]];
      return [`The ${issue.check} check failed`, [issue.message]];
  }
}

/** Every issue as a markdown list, trimmed to 1000 characters like `issuesAsMarkdown` in View.fs. */
function issuesAsMarkdown(issues: FormatIssue[]): string {
  const text = issues
    .map((issue) => {
      const [heading, lines] = describeIssue(issue);
      return `- **${heading}**\n${lines.map((l) => `  - ${l}`).join("\n")}`;
    })
    .join("\n");
  return text.length <= 1000 ? text : `${text.slice(0, 1000)}\n\n(truncated)`;
}

/** The fantomas-tools link that reopens this sample. Only changed settings travel: the tool fills in
 * the rest from its defaults, and every setting left out keeps the issue link under GitHub's limit. */
function sampleLink(): string {
  const data = LZString.compressToEncodedURIComponent(
    JSON.stringify({ code, settings: changed, isFsi: values.fsi }),
  );
  return `${toolsUrl}?${new URLSearchParams({ data })}`;
}

function buildBody(outcome: Exclude<Outcome, { kind: "invalid-source" }>, description: string | null): string {
  const codeTemplate = (header: string, content: string) => `
#### ${header}

\`\`\`fsharp
${content}
\`\`\`
            `;

  const result = outcome.kind === "error" ? null : outcome.result;
  const issues = result?.issues ?? [];
  const onlyIssue = issues.length === 1 ? issues[0] : undefined;

  let left: string, right: string;
  if (result && onlyIssue?.kind === "notIdempotent") {
    // fantomas-tools leaves the input out of this template and only links to it. Shown here too, as a
    // reader cannot tell which change is the problem without seeing where the first format started.
    left = codeTemplate("Code", code) + "\n" + codeTemplate("Formatted code", result.code);
    right = codeTemplate("Reformatted code", onlyIssue.formattedAgain);
  } else {
    const [header, content] =
      outcome.kind === "error"
        ? ["Error", outcome.message.length <= 1000 ? outcome.message : `${outcome.message.slice(0, 1000)}\n\n(truncated)`]
        : ["Result", outcome.result.code];
    left = codeTemplate("Code", code);
    right = codeTemplate(header, content);
  }

  const fallbackDescription = `Please describe here the Fantomas problem you encountered.
                    Check out our [Contribution Guidelines](https://github.com/fsprojects/fantomas/blob/main/CONTRIBUTING.md#bug-reports).`;
  // What Fantomas found wrong with its own result goes first, the way fantomas-tools starts its issue.
  const problem =
    issues.length === 0
      ? (description ?? fallbackDescription)
      : `Fantomas found this wrong with its result:\n\n${issuesAsMarkdown(issues)}\n\n${description ?? fallbackDescription}`;

  const options =
    changed.length === 0
      ? "Default Fantomas configuration"
      : "```fsharp\n    { config with\n" +
        changed.map((o) => `                ${o.$value[1]} = ${String(o.$value[2])}`).join("\n") +
        " }\n```";

  // Output that does not parse is a fact the backend established, so that box is ticked here.
  // The other three are for the reporter to decide in the browser.
  const breaks = issues.some((i) => i.kind === "notValidFSharp") ? "x" : " ";

  return `
<!--

    Please only use this to create issues.
    If you wish to suggest a feature,
    please fill in the feature request template at https://github.com/fsprojects/fantomas/issues/new/choose

-->
Issue created from [fantomas-online](${sampleLink()})

${left}
${right}
#### Problem description

${problem}

#### Extra information

- [${breaks}] The formatted result breaks my code.
- [ ] The formatted result gives compiler warnings.
- [ ] I or my company would be willing to help fix this.
- [ ] I would like a release if this problem is solved.

#### Options

Fantomas ${version}

${options}
${values.fsi ? "\n*Signature file*" : ""}

<sub>Did you know that you can ignore files when formatting by using a [.fantomasignore file](https://fsprojects.github.io/fantomas/docs/end-users/IgnoreFiles.html)?</sub>
<sub>PS: It's unlikely that someone else will solve your specific issue, as it's something that you have a personal stake in.</sub>
        `;
}

function openIssue(title: string, body: string) {
  const newIssue = "https://github.com/fsprojects/fantomas/issues/new";
  const bodyFile = join(tmpdir(), "fantomas-issue-body.md");
  writeFileSync(bodyFile, body);

  const full = `${newIssue}?title=${encodeURIComponent(title)}&body=${encodeURIComponent(body)}`;
  let url = full;
  console.log(`body: ${bodyFile}`);
  console.log(`issue link length: ${full.length} of ${maxIssueUrlLength}`);

  if (full.length > maxIssueUrlLength) {
    // Too long for GitHub. Open the form with the title only and hand over the body another way.
    url = `${newIssue}?title=${encodeURIComponent(title)}`;
    const copied = copyToClipboard(body);
    console.log(
      copied
        ? "too long for a link: the body is on the clipboard, paste it into the form"
        : `too long for a link: paste the body from ${bodyFile} into the form`,
    );
  }

  console.log(url);
  if (!values["no-open"]) {
    const opener = process.platform === "darwin" ? "open" : process.platform === "win32" ? "explorer" : "xdg-open";
    spawnSync(opener, [url], { stdio: "ignore" });
  }
}

function copyToClipboard(text: string): boolean {
  const candidates: [string, string[]][] =
    process.platform === "darwin"
      ? [["pbcopy", []]]
      : process.platform === "win32"
        ? [["clip", []]]
        : [
            ["wl-copy", []],
            ["xclip", ["-selection", "clipboard"]],
          ];
  return candidates.some(([cmd, args]) => spawnSync(cmd, args, { input: text }).status === 0);
}
