#!/usr/bin/env python3
"""
Consistency checks for FFI docs against their .ffi sources.

Currently checked pairs:
- DOCS-standard.md vs ffi/standard.ffi
- DOCS-dom.md vs ffi/dom.ffi
- DOCS-js.md vs ffi/js.ffi
- DOCS-math.md vs ffi/math.ffi
- DOCS-jsxgraph.md vs ffi/jsxgraph.ffi
- DOCS-xtermjs.md vs ffi/xtermjs.ffi

Checks per document:
- every documented table function exists in the target .ffi
- every table function is linked to an allowed docs URL prefix
- no duplicate function rows in tables
- documented coverage count matches table rows
- table rows cover all non-legacy ffi functions
- Table of Contents links cover all section headings (## and ###)
- optional strict table checks (non-empty Example/Use when cells)
- optional strict index checks (alphabetized index uniqueness + coverage)
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
FN_PATTERN = r"(?:js|jsx|xterm)-[^`)\s]+"


@dataclass(frozen=True)
class DocCheckConfig:
    label: str
    doc_path: Path
    ffi_path: Path
    coverage_patterns: tuple[str, ...]
    legacy_section_heading_pattern: str | None = None
    require_table_example_use_when: bool = False
    require_index_coverage: bool = False
    required_function_table_columns: tuple[str, ...] = ()
    allowed_url_prefixes: tuple[str, ...] = ("https://developer.mozilla.org/",)


CONFIGS: tuple[DocCheckConfig, ...] = (
    DocCheckConfig(
        label="standard",
        doc_path=ROOT / "DOCS-standard.md",
        ffi_path=ROOT / "ffi" / "standard.ffi",
        coverage_patterns=(
            r"This document covers \*\*(\d+)\*\* functions from `ffi/standard\.ffi`\.",
        ),
        legacy_section_heading_pattern=r"^### 2\.8 Legacy Wrappers\s*$",
    ),
    DocCheckConfig(
        label="dom",
        doc_path=ROOT / "DOCS-dom.md",
        ffi_path=ROOT / "ffi" / "dom.ffi",
        coverage_patterns=(
            r"- Total documented functions: \*\*(\d+)\*\*",
            r"This document covers \*\*(\d+)\*\* functions from `ffi/dom\.ffi`\.",
        ),
        legacy_section_heading_pattern=None,
        require_table_example_use_when=True,
        require_index_coverage=False,
        required_function_table_columns=(
            "Side effects?",
            "Callback?",
            "Nullable return?",
        ),
    ),
    DocCheckConfig(
        label="js",
        doc_path=ROOT / "DOCS-js.md",
        ffi_path=ROOT / "ffi" / "js.ffi",
        coverage_patterns=(
            r"- Total documented functions: \*\*(\d+)\*\*",
            r"This document covers \*\*(\d+)\*\* functions from `ffi/js\.ffi`\.",
        ),
        legacy_section_heading_pattern=None,
        require_table_example_use_when=True,
        require_index_coverage=False,
    ),
    DocCheckConfig(
        label="math",
        doc_path=ROOT / "DOCS-math.md",
        ffi_path=ROOT / "ffi" / "math.ffi",
        coverage_patterns=(
            r"- Total documented functions: \*\*(\d+)\*\*",
            r"This document covers \*\*(\d+)\*\* functions from `ffi/math\.ffi`\.",
        ),
        legacy_section_heading_pattern=None,
        require_table_example_use_when=True,
        require_index_coverage=False,
    ),
    DocCheckConfig(
        label="jsxgraph",
        doc_path=ROOT / "DOCS-jsxgraph.md",
        ffi_path=ROOT / "ffi" / "jsxgraph.ffi",
        coverage_patterns=(
            r"- Total documented functions: \*\*(\d+)\*\*",
            r"This document covers \*\*(\d+)\*\* functions from `ffi/jsxgraph\.ffi`\.",
        ),
        legacy_section_heading_pattern=None,
        require_table_example_use_when=True,
        require_index_coverage=False,
        allowed_url_prefixes=("https://jsxgraph.org/docs/",),
    ),
    DocCheckConfig(
        label="xtermjs",
        doc_path=ROOT / "DOCS-xtermjs.md",
        ffi_path=ROOT / "ffi" / "xtermjs.ffi",
        coverage_patterns=(
            r"- Total documented functions: \*\*(\d+)\*\*",
            r"This document covers \*\*(\d+)\*\* functions from `ffi/xtermjs\.ffi`\.",
        ),
        legacy_section_heading_pattern=None,
        require_table_example_use_when=True,
        require_index_coverage=False,
        allowed_url_prefixes=("https://xtermjs.org/docs/",),
    ),
)


def extract_ffi_functions(text: str) -> list[str]:
    return re.findall(rf"^\(define-foreign\s+({FN_PATTERN})", text, flags=re.MULTILINE)


def extract_doc_table_rows(text: str) -> list[tuple[str, str]]:
    # Matches table rows where Function column is a markdown link:
    # | [`js-foo`](https://...) | ...
    return re.findall(
        rf"^\|\s*\[`({FN_PATTERN})`\]\((https?://[^)]+)\)\s*\|",
        text,
        flags=re.MULTILINE,
    )


def parse_table_cells(line: str) -> list[str]:
    if not line.startswith("|"):
        return []
    body = line.strip()
    if body.endswith("|"):
        body = body[:-1]
    if body.startswith("|"):
        body = body[1:]
    return [cell.strip() for cell in body.split("|")]


def iter_markdown_tables(text: str) -> list[tuple[list[str], list[list[str]]]]:
    lines = text.splitlines()
    out: list[tuple[list[str], list[list[str]]]] = []
    i = 0
    while i + 1 < len(lines):
        header = lines[i]
        divider = lines[i + 1]
        if header.startswith("|") and divider.startswith("|"):
            divider_cells = parse_table_cells(divider)
            if divider_cells and all(re.fullmatch(r"[:\-\s]+", c) for c in divider_cells):
                headers = parse_table_cells(header)
                rows: list[list[str]] = []
                j = i + 2
                while j < len(lines) and lines[j].startswith("|"):
                    rows.append(parse_table_cells(lines[j]))
                    j += 1
                out.append((headers, rows))
                i = j
                continue
        i += 1
    return out


def extract_index_functions(text: str) -> list[str]:
    m = re.search(
        r"^## .*Alphabetized Index\s*$\n(.*?)(?=^## |\Z)",
        text,
        flags=re.MULTILINE | re.DOTALL,
    )
    if not m:
        return []
    section = m.group(1)
    linked = re.findall(rf"^\s*-\s+\[`({FN_PATTERN})`\]\(", section, flags=re.MULTILINE)
    plain = re.findall(rf"^\s*-\s+`({FN_PATTERN})`(?:\s|$)", section, flags=re.MULTILINE)
    if linked:
        return linked
    return plain


def extract_legacy_functions(text: str, section_heading_pattern: str | None) -> set[str]:
    if not section_heading_pattern:
        return set()

    m = re.search(
        section_heading_pattern + r"\n(.*?)(?=^### |\n## |\Z)",
        text,
        flags=re.MULTILINE | re.DOTALL,
    )
    if not m:
        return set()

    section = m.group(1)
    # Only treat bullet-listed wrappers as legacy; ignore advisory mentions.
    return set(re.findall(rf"^\s*-\s*`({FN_PATTERN})`", section, flags=re.MULTILINE))


def extract_coverage_count(text: str, patterns: tuple[str, ...]) -> int | None:
    for pattern in patterns:
        m = re.search(pattern, text)
        if m:
            return int(m.group(1))
    return None


def heading_anchor(label: str) -> str:
    # Approximate GitHub markdown anchor generation for this document's headings.
    lowered = label.lower()
    cleaned = "".join(ch for ch in lowered if (ch.isalnum() or ch in {" ", "-"}))
    return "#" + cleaned.replace(" ", "-").strip("-")


def extract_headings(text: str) -> list[str]:
    heads = re.findall(r"^(##|###)\s+(.+?)\s*$", text, flags=re.MULTILINE)
    labels = [label for _level, label in heads]
    return [h for h in labels if h != "Table of Contents"]


def extract_toc_anchors(text: str) -> list[str]:
    m = re.search(
        r"^### Table of Contents\s*$\n(.*?)(?=^### |\n## |\Z)",
        text,
        flags=re.MULTILINE | re.DOTALL,
    )
    if not m:
        return []

    block = m.group(1)
    return re.findall(r"^\s*-\s+\[[^\]]+\]\((#[^)]+)\)\s*$", block, flags=re.MULTILINE)


def run_check(cfg: DocCheckConfig) -> tuple[list[str], tuple[int, int, int]]:
    errors: list[str] = []

    if not cfg.doc_path.exists():
        return ([f"[{cfg.label}] missing file: {cfg.doc_path}"] , (0, 0, 0))
    if not cfg.ffi_path.exists():
        return ([f"[{cfg.label}] missing file: {cfg.ffi_path}"] , (0, 0, 0))

    doc_text = cfg.doc_path.read_text(encoding="utf-8")
    ffi_text = cfg.ffi_path.read_text(encoding="utf-8")

    ffi_funcs = extract_ffi_functions(ffi_text)
    ffi_set = set(ffi_funcs)

    rows = extract_doc_table_rows(doc_text)
    doc_funcs = [name for name, _url in rows]
    doc_set = set(doc_funcs)

    legacy_set = extract_legacy_functions(doc_text, cfg.legacy_section_heading_pattern)
    expected_doc_set = ffi_set - legacy_set

    # Duplicate function rows in docs tables.
    seen: set[str] = set()
    dups: list[str] = []
    for f in doc_funcs:
        if f in seen:
            dups.append(f)
        seen.add(f)
    if dups:
        errors.append(f"[{cfg.label}] duplicate documented table function rows: {sorted(set(dups))}")

    # Ensure no old unlinked function rows remain in function tables.
    unlinked_rows: list[str] = []
    for headers, rows_cells in iter_markdown_tables(doc_text):
        lower = [h.lower() for h in headers]
        if "function" not in lower:
            continue
        fn_idx = lower.index("function")
        for row in rows_cells:
            if len(row) <= fn_idx:
                continue
            fn_cell = row[fn_idx]
            m_unlinked = re.fullmatch(rf"`({FN_PATTERN})`", fn_cell)
            if m_unlinked:
                unlinked_rows.append(m_unlinked.group(1))
    if unlinked_rows:
        errors.append(
            f"[{cfg.label}] unlinked table function rows found (expected markdown links): "
            f"{sorted(set(unlinked_rows))}"
        )

    # Ensure URLs use allowed documentation prefixes.
    bad_urls = [
        (fn, url)
        for fn, url in rows
        if not any(url.startswith(prefix) for prefix in cfg.allowed_url_prefixes)
    ]
    if bad_urls:
        errors.append(
            f"[{cfg.label}] disallowed function links found: "
            + ", ".join(f"{fn} -> {url}" for fn, url in bad_urls)
        )

    if cfg.require_table_example_use_when:
        table_errors: list[str] = []
        for headers, rows_cells in iter_markdown_tables(doc_text):
            if not headers:
                continue
            lower = [h.lower() for h in headers]
            if "function" not in lower:
                continue
            if "example" not in lower or "use when" not in lower:
                table_errors.append(
                    f"table with Function column is missing Example/Use when columns: headers={headers}"
                )
                continue
            if cfg.required_function_table_columns:
                missing_cols = [c for c in cfg.required_function_table_columns if c.lower() not in lower]
                if missing_cols:
                    table_errors.append(
                        f"table with Function column is missing required columns {missing_cols}: headers={headers}"
                    )
                    continue
            fn_idx = lower.index("function")
            ex_idx = lower.index("example")
            use_idx = lower.index("use when")
            for row in rows_cells:
                if len(row) <= max(fn_idx, ex_idx, use_idx):
                    continue
                fn_cell = row[fn_idx]
                if not re.fullmatch(rf"\[`{FN_PATTERN}`\]\(https?://[^)]+\)", fn_cell):
                    continue
                if not row[ex_idx].strip():
                    table_errors.append(f"empty Example cell for {fn_cell}")
                if not row[use_idx].strip():
                    table_errors.append(f"empty Use when cell for {fn_cell}")
        if table_errors:
            errors.append(f"[{cfg.label}] table quality issues: {table_errors}")

    # Documented table functions must exist in ffi.
    unknown = sorted(doc_set - ffi_set)
    if unknown:
        errors.append(f"[{cfg.label}] documented table functions missing in {cfg.ffi_path.name}: {unknown}")

    # Docs table should cover all non-legacy ffi functions.
    missing = sorted(expected_doc_set - doc_set)
    extra = sorted(doc_set - expected_doc_set)
    if missing:
        errors.append(f"[{cfg.label}] ffi functions missing from docs tables (excluding legacy): {missing}")
    if extra:
        errors.append(f"[{cfg.label}] unexpected functions in docs tables: {extra}")

    coverage = extract_coverage_count(doc_text, cfg.coverage_patterns)
    if coverage is None:
        errors.append(f"[{cfg.label}] coverage line not found in {cfg.doc_path.name}")
    else:
        if coverage != len(doc_set):
            errors.append(
                f"[{cfg.label}] coverage count mismatch: docs says {coverage}, "
                f"table rows are {len(doc_set)}"
            )
        if coverage != len(expected_doc_set):
            errors.append(
                f"[{cfg.label}] coverage count mismatch against {cfg.ffi_path}: "
                f"expected {len(expected_doc_set)} "
                f"(total {len(ffi_set)} minus legacy {len(legacy_set)}), got {coverage}"
            )

    # TOC should cover all ## and ### headings (excluding the TOC heading itself).
    toc_anchors = extract_toc_anchors(doc_text)
    if not toc_anchors:
        errors.append(f"[{cfg.label}] Table of Contents not found or contains no links")
    else:
        expected_anchors = [heading_anchor(h) for h in extract_headings(doc_text)]
        expected_set = set(expected_anchors)
        toc_set = set(toc_anchors)

        missing_toc = sorted(expected_set - toc_set)
        extra_toc = sorted(toc_set - expected_set)
        if missing_toc:
            errors.append(f"[{cfg.label}] TOC missing section anchors: {missing_toc}")
        if extra_toc:
            errors.append(f"[{cfg.label}] TOC has unknown anchors: {extra_toc}")

        if len(toc_anchors) != len(toc_set):
            errors.append(f"[{cfg.label}] TOC has duplicate anchor entries")

    if cfg.require_index_coverage:
        index_funcs = extract_index_functions(doc_text)
        if not index_funcs:
            errors.append(f"[{cfg.label}] Alphabetized Index section not found or empty")
        else:
            seen_idx: set[str] = set()
            dup_idx: list[str] = []
            for f in index_funcs:
                if f in seen_idx:
                    dup_idx.append(f)
                seen_idx.add(f)
            if dup_idx:
                errors.append(
                    f"[{cfg.label}] duplicate function entries in Alphabetized Index: {sorted(set(dup_idx))}"
                )
            index_set = set(index_funcs)
            missing_idx = sorted(doc_set - index_set)
            extra_idx = sorted(index_set - doc_set)
            if missing_idx:
                errors.append(f"[{cfg.label}] Alphabetized Index missing functions: {missing_idx}")
            if extra_idx:
                errors.append(f"[{cfg.label}] Alphabetized Index has unknown functions: {extra_idx}")

    return errors, (len(ffi_set), len(legacy_set), len(doc_set))


def main() -> int:
    all_errors: list[str] = []
    stats: list[tuple[str, int, int, int]] = []

    for cfg in CONFIGS:
        errors, (ffi_count, legacy_count, doc_count) = run_check(cfg)
        all_errors.extend(errors)
        stats.append((cfg.label, ffi_count, legacy_count, doc_count))

    if all_errors:
        print("FFI docs checks FAILED")
        for e in all_errors:
            print(f"- {e}")
        return 1

    print("FFI docs checks OK")
    for label, ffi_count, legacy_count, doc_count in stats:
        print(f"- {label}: ffi={ffi_count}, legacy={legacy_count}, documented={doc_count}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
