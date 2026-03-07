# Bibliography Verification Report

**Date**: 2026-03-04
**File**: `sn-bibliography.bib`
**Scope**: 17 entries added after `%% === New references added in revision ===`

---

## Summary

| Status | Count | Details |
|--------|------:|---------|
| Correct | 5 | No changes needed |
| Wrong metadata | 8 | Authors, DOI, or both incorrect |
| Fabricated | 1 | Paper does not exist |
| Unused (never cited) | 3 | Can be removed from bib |

---

## Correct Entries (5)

| Key | Authors | Venue | Year |
|-----|---------|-------|------|
| `probst2018` | Probst, Philipp; Boulesteix, Anne-Laure | JMLR | 2018 |
| `morris2019` | Morris, Tim P.; White, Ian R.; Crowther, Michael J. | Statistics in Medicine | 2019 |
| `janitza2018` | Janitza, Silke; Hornung, Roman | PLOS ONE | 2018 |
| `mitchell2011` | Mitchell, Matthew W. | Open Journal of Statistics | 2011 |
| `fawcett2006` | Fawcett, Tom | Pattern Recognition Letters | 2006 |

---

## Wrong Metadata — Corrections Applied (8)

### 1. `farhadi2023` → renamed to `sun2024`

| Field | Old (wrong) | New (correct) |
|-------|-------------|---------------|
| **Key** | `farhadi2023` | `sun2024` |
| **Authors** | Farhadi, Farzaneh; Jafari Navimipour, Nima; Seyedi, Mehdi; Arasteh, Bahman | Sun, Zhigang; Wang, Guotao; Li, Pengfei; Wang, Hui; Zhang, Min; Liang, Xiaowen |
| **Year** | 2023 | 2024 |
| **Volume** | 227 | 237 |
| **Pages** | 120282 | 121549 |
| **DOI** | 10.1016/j.eswa.2023.120282 (→ breast cancer paper by Fei Yan) | 10.1016/j.eswa.2023.121549 |

**Note**: The old DOI resolves to a completely different paper. All `\cite{farhadi2023}` updated to `\cite{sun2024}` in main.tex.

### 2. `tan2024`

| Field | Old (wrong) | New (correct) |
|-------|-------------|---------------|
| **Authors** | Tan, Yan Shuo; Singh, Abhineet; Agarwal, Anant; Solus, Liam | Curth, Alicia; Jeffares, Alan; van der Schaar, Mihaela |

**Source**: [arXiv:2402.01502](https://arxiv.org/abs/2402.01502)

### 3. `yang2024auroc`

| Field | Old (wrong) | New (correct) |
|-------|-------------|---------------|
| **Authors** | Yang, Jiacheng; Xu, Yong; Zou, James | McDermott, Matthew B. A.; Hyldig Hansen, Lasse; Zhang, Haoran; Angelotti, Giovanni; Gallifant, Jack |

**Source**: [arXiv:2401.06091](https://arxiv.org/abs/2401.06091), [NeurIPS 2024 proceedings](https://proceedings.neurips.cc/paper_files/paper/2024/hash/4df3510ad02a86d69dc32388d91606f8-Abstract-Conference.html)

### 4. `richardson2024`

| Field | Old (wrong) | New (correct) |
|-------|-------------|---------------|
| **Authors** | Richardson, Eve; Trevino, Robert (2 authors) | Richardson, Eve; Trevizani, Raphael; Greenbaum, Jason A.; Carter, Hannah; Nielsen, Morten; Peters, Bjoern (6 authors) |

**Note**: Surname "Trevino" was misspelled (correct: "Trevizani"), and 4 co-authors were missing.
**Source**: [PubMed 39005487](https://pubmed.ncbi.nlm.nih.gov/39005487/)

### 5. `nikolaou2023`

| Field | Old (wrong) | New (correct) |
|-------|-------------|---------------|
| **Authors** | Nikolaou, Nikos; Edakunni, Narayanan U.; Kull, Meelis; Flach, Peter A.; Brown, Gavin | Iosifidis, Vasileios; Papadopoulos, Symeon; Rosenhahn, Bodo; Ntoutsi, Eirini |

**Note**: The old authors wrote a *different* paper: "Cost-sensitive boosting algorithms: Do we really need them?" (Machine Learning, 2016). They did NOT write AdaCC.
**Source**: [Springer](https://link.springer.com/article/10.1007/s10115-022-01780-8)

### 6. `dablain2023` → renamed to `johnson2022`

| Field | Old (wrong) | New (correct) |
|-------|-------------|---------------|
| **Key** | `dablain2023` | `johnson2022` |
| **Authors** | Dablain, Damien; Krawczyk, Bartosz; Chawla, Nitesh V. | Johnson, Justin M.; Khoshgoftaar, Taghi M. |
| **Year** | 2023 | 2022 |
| **Booktitle** | 2023 IEEE International Conference on Big Data | 2022 International Joint Conference on Neural Networks (IJCNN) |
| **DOI** | 10.1109/BigData59044.2023.10386190 (→ LLM weak supervision paper) | 10.1109/IJCNN55027.2022.10069008 |

**Note**: The old DOI resolves to "Leveraging Large Language Models for Structure Learning in Prompted Weak Supervision" — completely unrelated. Dablain, Krawczyk, and Chawla are real researchers but did NOT author this paper.
**Source**: [IEEE Xplore](https://ieeexplore.ieee.org/document/10069008/)

### 7. `zhu2024survey` → renamed to `chen2024survey`

| Field | Old (wrong) | New (correct) |
|-------|-------------|---------------|
| **Key** | `zhu2024survey` | `chen2024survey` |
| **Authors** | Zhu, Jian; Wang, Jiaqi; Chen, Yiwen; Ye, Haomin | Chen, Wuxing; Yang, Kaixiang; Yu, Zhiwen; Shi, Yifan; Chen, C. L. Philip |

**Note**: DOI (10.1007/s10462-024-10759-6) was correct.
**Source**: [Springer](https://link.springer.com/article/10.1007/s10462-024-10759-6)

### 8. `lange2025`

| Field | Old (wrong) | New (correct) |
|-------|-------------|---------------|
| **Authors** | Lange, Tino M.; Rach, Stefan; Jahnke, Tanja | Lange, Thomas M.; Gültas, Mehmet; Schmitt, Armin O.; Heinrich, Felix |
| **Pages** | 42 | 95 |
| **DOI** | 10.1186/s12859-025-06065-7 | 10.1186/s12859-025-06097-1 |

**Source**: [BMC Bioinformatics](https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-025-06097-1)

---

## Fabricated Entry — Removed (1)

### `hornung2024`

- **Claimed**: "Prediction Error Estimation in Random Forests" by Hornung, Roman; Hapfelmeier, Alexander. arXiv:2407.08344.
- **Reality**: arXiv:2407.08344 is **"Quantum Thermodynamic Integrability for Canonical and non-Canonical Statistics"** by Ruo-Xun Zhai and C.P. Sun — a quantum physics paper.
- **Extensive search** found no paper by Hornung & Hapfelmeier with this title. A paper with a similar title (arXiv:2309.00736 by Krupkin & Hardin) exists but was **withdrawn** due to methodological issues.
- **Action**: Entry removed from bib; citation and surrounding sentence removed from main.tex.

---

## Unused Entries — Removed (3)

| Key | Problem | Status |
|-----|---------|--------|
| `haixiang2017` | Correct metadata, but never cited in main.tex | Removed |
| `marques2025` | Wrong authors (Marques → real: Carvalho, Pinho, Bras); wrong DOI; never cited | Removed |
| `liu2025climb` | Wrong authors (Liu, Zongyi → real: Zhining Liu et al.); never cited | Removed |

---

## Files Modified

1. `sn-bibliography.bib` — 8 entries corrected, 4 entries removed
2. `main.tex` — 3 `\cite{}` keys updated, 1 sentence removed
3. `response202603.tex` — reference table updated
