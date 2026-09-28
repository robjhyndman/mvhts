# Lessons

- 2026-09-25: Don't commit.
  Rob commits himself; stage changes with `git add` and stop there.
  Permission to commit on your own covered only the overnight solo run on 2026-09-24.
  When a permission was granted for one specific situation, check that situation still applies before using it.
- 2026-09-25: The paper must read linearly.
  No forward references except the outline in the intro, or a brief note explaining why background material is being covered.
  Before placing a figure or result, check that every term it uses (designs, hierarchies, families, acronyms) has already been defined.
  Methods sections define; evaluation of methods belongs where the evaluation designs are introduced.
  Never refer to "the original design of this study" or other earlier drafts.
- 2026-09-25: Abstracts go in the present tense.
  Rob's 2009 Hyndsight post on abstracts says past tense, but he no longer holds that view.
  Follow the rest of that post (what/why/how/results/implication, one paragraph, no citations) but write in the present tense.
- 2026-09-25: Check `~/.ssh/config` before giving rsync/scp commands to Rob's hosts.
  `desktop`, `laptop` and `tvpc` force `RemoteCommand zsh -l`, so plain rsync fails with code 255.
  Always add `-e "ssh -o RemoteCommand=none -o RequestTTY=no"`.
- 2026-09-28: No results in the introduction.
  The intro sets out the problem, the gap in the literature and the approach (methods, experiments, data), then the outline.
  Findings, numbers and verdicts belong in the abstract and in the later sections, not in the intro.
  Known results from the literature are background and can stay; the paper's own theorems are described by what they deliver, not stated.
