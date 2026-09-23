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
type FormatResponse = {
  firstFormat: string;
  firstValidation: Diagnostic[];
  secondFormat: string | null;
  secondValidation: Diagnostic[];
};
type Outcome =
  | { kind: "invalid-source"; message: string }
  | { kind: "error"; message: string }
  | { kind: "invalid-output"; result: FormatResponse }
  | { kind: "not-idempotent"; result: FormatResponse }
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
    const message = tryParse(text)?.message ?? text;
    return { kind: response.status === 400 ? "invalid-source" : "error", message };
  }
  const result = JSON.parse(text) as FormatResponse;
  if (result.firstValidation.length > 0) return { kind: "invalid-output", result };
  if (result.secondFormat !== null && result.secondFormat !== result.firstFormat) {
    return { kind: "not-idempotent", result };
  }
  return { kind: "formatted", result };
}

function tryParse(text: string): { message?: string } | null {
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
    case "invalid-output":
      console.log(outcome.result.firstFormat);
      for (const d of outcome.result.firstValidation) {
        console.log(`(${d.range.startLine},${d.range.startColumn}) FS${d.errorNumber}: ${d.message}`);
      }
      break;
    case "not-idempotent":
      console.log(`--- first format\n${outcome.result.firstFormat}`);
      console.log(`--- second format\n${outcome.result.secondFormat}`);
      break;
    case "formatted":
      console.log(outcome.result.firstFormat);
      break;
  }
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

  let left: string, right: string, fallbackDescription: string;
  if (outcome.kind === "not-idempotent") {
    // fantomas-tools leaves the input out of this template and only links to it. Shown here too, as a
    // reader cannot tell which change is the problem without seeing where the first format started.
    left =
      codeTemplate("Code", code) +
      "\n" +
      codeTemplate("Formatted code", outcome.result.firstFormat);
    right = codeTemplate("Reformatted code", outcome.result.secondFormat!);
    fallbackDescription = "Fantomas was not able to produce the same code after reformatting the result.";
  } else {
    const [header, content] =
      outcome.kind === "error"
        ? ["Error", outcome.message.length <= 1000 ? outcome.message : `${outcome.message.slice(0, 1000)}\n\n(truncated)`]
        : ["Result", outcome.result.secondFormat ?? outcome.result.firstFormat];
    left = codeTemplate("Code", code);
    right = codeTemplate(header, content);
    fallbackDescription = `Please describe here the Fantomas problem you encountered.
                    Check out our [Contribution Guidelines](https://github.com/fsprojects/fantomas/blob/main/CONTRIBUTING.md#bug-reports).`;
  }

  const options =
    changed.length === 0
      ? "Default Fantomas configuration"
      : "```fsharp\n    { config with\n" +
        changed.map((o) => `                ${o.$value[1]} = ${String(o.$value[2])}`).join("\n") +
        " }\n```";

  // Output that does not parse is a fact the backend established, so that box is ticked here.
  // The other three are for the reporter to decide in the browser.
  const breaks = outcome.kind === "invalid-output" ? "x" : " ";

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

${description ?? fallbackDescription}

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
