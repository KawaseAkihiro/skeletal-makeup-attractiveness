# Data and code for “Skeletal Structure and Makeup Effects in Facial Attractiveness Judgments”

This repository contains the public data files associated with the article “Skeletal Structure and Makeup Effects in Facial Attractiveness Judgments,” accepted for publication in *Perception*.

## Contents

* `rating_data_anonymized.csv`: anonymized facial-attractiveness rating data.
* `derived_face_measures.csv`: study-specific anonymized face IDs, ethnicity categories, derived facial-shape measures, skeletal attractiveness group labels, and skeletal type codes.
* `codebook.csv`: variable descriptions for the public data files.

## Data restrictions

The original face photographs were obtained from the Chicago Face Database (CFD) and are subject to its data-use agreement. Therefore, the original images, rendered stimulus images, 3D meshes, raw landmark coordinates, and identifiers linking the derived measures to the original CFD model IDs are not redistributed.

The variable `face_id` is a study-specific anonymized identifier used only to link `rating_data_anonymized.csv` and `derived_face_measures.csv`. It does not correspond to the original CFD model ID, and no mapping to the original CFD identifiers is provided in the public repository.

## Measures

The variables `r_mid` and `r_jaw_adj` are projection-based facial-shape measures derived from model-based landmark positions. The variables `z_r_mid` and `z_r_jaw_adj` are robust z-scores computed within each ethnicity category using the median and median absolute deviation (MAD).

The variable `skeletal_group` indicates the skeletal attractiveness group used in the main analyses. The variable `type_code` indicates the skeletal-profile classification code used in the study.

## Ethnicity labels

The ethnicity categories in the public data are `Asian American`, `Black`, `Latina`, and `White`. The label `Latina` is used consistently because the face stimuli analyzed in this study were female faces.

## Reproducibility

The shared files allow reproduction of the reported analyses from the anonymized rating data and the derived facial-shape measures. The original CFD photographs, rendered stimulus images, 3D meshes, raw landmark coordinates, and mapping between the anonymized `face_id` values and original CFD model IDs are not required for reproducing the reported statistical analyses and are not redistributed.

## Citation

Citation

If you use these data or code, please cite the associated article:

Akihiro Kawase and Rikuto Yamamoto. (in press). Skeletal structure and makeup effects in facial attractiveness judgments. Perception. https://doi.org/10.1177/03010066261459982

The bibliographic details, including publication year, volume, issue, and page/article number, will be updated once the article is formally published.
