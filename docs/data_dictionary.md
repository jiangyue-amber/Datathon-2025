# Skin Cancer Dataset Dictionary

## Target Variable

| Field Name | Description | Values |
|------------|-------------|--------|
| `target` | Binary classification target indicating malignancy status | `0`: benign, `1`: malignant |

## Patient Information

| Field Name | Description | Type |
|------------|-------------|------|
| `isic_id` | Unique identifier for each case/image | string |
| `patient_id` | Unique identifier for each patient | string |
| `age_approx` | Approximate age of patient at time of imaging | numeric |
| `sex` | Sex of the person | categorical |

## Lesion Location & Physical Characteristics

| Field Name | Description | Type | Range/Units |
|------------|-------------|------|-------------|
| `anatom_site_general` | Anatomical location of the lesion | categorical | e.g., face, trunk, upper extremity |
| `clin_size_long_diam_mm` | Maximum diameter of the lesion | numeric | millimeters |
| `tbp_lv_areaMM2` | Area of the lesion | numeric | square millimeters |
| `tbp_lv_perimeterMM` | Perimeter of the lesion | numeric | millimeters |
| `tbp_lv_minorAxisMM` | Smallest lesion diameter | numeric | millimeters |
| `tbp_lv_location` | Detailed anatomical location classification | categorical | - |
| `tbp_lv_location_simple` | Simplified anatomical location classification | categorical | - |

## Lesion Morphological Features

| Field Name | Description | Type | Range/Units |
|------------|-------------|------|-------------|
| `tbp_lv_area_perim_ratio` | Border jaggedness (ratio of perimeter to area) | numeric | 0-10, higher = more irregular |
| `tbp_lv_eccentricity` | Measure of lesion shape eccentricity | numeric | - |
| `tbp_lv_norm_border` | Normalized border irregularity | numeric | 0-10 scale |
| `tbp_lv_symm_2axis` | Border asymmetry measure | numeric | 0-10, higher = more asymmetric |
| `tbp_lv_symm_2axis_angle` | Angle of lesion border asymmetry | numeric | degrees |

## Color & Contrast Features

| Field Name | Description | Type | Range/Units |
|------------|-------------|------|-------------|
| `tbp_lv_L` | L (lightness) value inside lesion (LAB color space) | numeric | - |
| `tbp_lv_Lext` | L value outside lesion | numeric | - |
| `tbp_lv_A` | A value inside lesion (LAB color space) | numeric | - |
| `tbp_lv_Aex` | A value outside lesion | numeric | - |
| `tbp_lv_B` | B value inside lesion (LAB color space) | numeric | - |
| `tbp_lv_Bext` | B value outside lesion | numeric | - |
| `tbp_lv_C` | Chroma inside lesion | numeric | - |
| `tbp_lv_Cext` | Chroma outside lesion | numeric | - |
| `tbp_lv_H` | Hue inside lesion | numeric | 25 (red) to 75 (brown) |
| `tbp_lv_Hext` | Hue outside lesion | numeric | - |
| `tbp_lv_deltaL` | L contrast (inside vs. outside) | numeric | - |
| `tbp_lv_deltaA` | A contrast (inside vs. outside) | numeric | - |
| `tbp_lv_deltaB` | B contrast (inside vs. outside) | numeric | - |
| `tbp_lv_deltaLBnorm` | Normalized contrast between lesion and surrounding skin | numeric | 5.5-25, higher = more contrast |
| `tbp_lv_stdL` | Standard deviation of L inside lesion | numeric | - |
| `tbp_lv_stdLExt` | Standard deviation of L outside lesion | numeric | - |
| `tbp_lv_norm_color` | Normalized color variation | numeric | 0-10 scale |
| `tbp_lv_color_std_mean` | Color irregularity within lesion boundary | numeric | - |
| `tbp_lv_radial_color_std_max` | Color asymmetry measure | numeric | 0-10, higher = more asymmetric |

## AI-Derived Confidence Scores

| Field Name | Description | Type | Range/Units |
|------------|-------------|------|-------------|
| `tbp_lv_dnn_lesion_confidence` | AI-derived lesion confidence score | numeric | 0-100 scale |
| `tbp_lv_nevi_confidence` | AI-derived probability of lesion being a nevus | numeric | 0-100 scale |

## Diagnostic Information 

| Field Name | Description | Type |
|------------|-------------|------|
| `lesion_id` | Unique identifier for manually tagged lesions of interest | string |
| `iddx_full` | Complete lesion diagnosis | categorical |
| `iddx_1` through `iddx_5` | Hierarchical levels of lesion diagnosis | categorical |
| `mel_mitotic_index` | Mitotic index for malignant melanomas | numeric |
| `mel_thick_mm` | Thickness/depth of melanoma invasion | numeric (mm) |

## Technical/Metadata Fields

| Field Name | Description | Type |
|------------|-------------|------|
| `image_type` | ISIC Archive image type classification | categorical |
| `tbp_tile_type` | Lighting modality of the 3D TBP source image | categorical |
| `tbp_lv_x` | X-coordinate of lesion on 3D TBP | numeric |
| `tbp_lv_y` | Y-coordinate of lesion on 3D TBP | numeric |
| `tbp_lv_z` | Z-coordinate of lesion on 3D TBP | numeric |
| `attribution` | Image source information | string |
| `copyright_license` | Copyright license information | string |