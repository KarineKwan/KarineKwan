# Import libraries
import pickle
import pandas as pd
import numpy as np
from pathlib import Path
import zipfile
import io
import re
from sklearn.datasets import load_svmlight_file
from sklearn.model_selection import train_test_split
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.inspection import permutation_importance
from sklearn.metrics import classification_report, f1_score, precision_score, recall_score
import matplotlib.pyplot as plt

# 1. Process raw data

# ***Please save the raw data to the same folder***
# ***Only save one zip file in the folder for the raw data file***
# __file__ is a special variable, it is like the id of the py.file
# Path(__file__) can locate your current file path
# .resolve() is to make the file path to be absolute path (ensures precise identification of current file)
# .parent is to get the upper level directory of the file (folder)
rawdata_folder = Path(__file__).resolve().parent
# search and get all the zip files in the folder
rawdata = list(rawdata_folder.glob("*.zip"))
if not rawdata:
    raise FileNotFoundError(f"There is not a zip file in {rawdata_folder}.")

dfs_to_merge = []
# zipfile.ZipFile(rawdata[0]) means the content that inside the first zip file in the folder
# the raw data is in svmlight / libsvm format
# byte_stream is to store the data and wrap it as a virtual, in memory file
# X is the risk score and y is the features
# Data dictionary shows that there are 483 features, so n_features=483
# insert "risk_score" in the first row first column
with zipfile.ZipFile(rawdata[0], "r") as files:
    for file_name in files.namelist():
        data = files.open(file_name).read()
        bytes_stream = io.BytesIO(data)
        X, y = load_svmlight_file(bytes_stream, n_features=483)
        df_temp = pd.DataFrame(X.toarray())
        df_temp.insert(0, "risk_score", y)
        dfs_to_merge.append(df_temp)

df = pd.concat(dfs_to_merge, axis=0, ignore_index=True)

# Load mapping file ***(Please only save one csv file in the folder)***
mapfile = list(rawdata_folder.glob("*.csv"))
if not mapfile:
    raise FileNotFoundError(f"There is not a csv file in {rawdata_folder}.")
df_map = pd.read_csv(mapfile[0])

# int(row["feature_number"]): row["feature_name"] is making the key-value relationship for the dictionary
# using int is because sometimes csv will make the row["feature_num"] be float
# _ is because we only need the feature name
# dropna is to prevent there are some empty rows in csv which will miss some features
# subset is to make it only check these two columns
columnnamesdict = {
    int(row["feature_number"]): row["feature_name"]
    for _, row in df_map.dropna(subset=["feature_number", "feature_name"]).iterrows()
}

final_df = df.rename(columns=columnnamesdict)
print(final_df.head())

# 2. Data Cleaning and Feature Creation

# Fill na except the first column (risk score) with 0
# Reason for features #368-482 also fill na with 0 although the data type is binary is because signature_features
# is the alarm system in Cuckoo Sandbox. If the raw data doesn't record anything, it should fill as 0
# iloc[:, 1:] means all rows and all columns except the first column (risk score)
final_df.iloc[:, 1:] = final_df.iloc[:, 1:].fillna(0)

# drop na if there is na on the first column
final_df = final_df.dropna(subset=['risk_score'])


# make sure column names are unique (remove any blank from the start or the end of the column name first)
final_df.columns = final_df.columns.astype(str).str.strip()

counts = final_df.columns.to_series().groupby(level=0).cumcount()
is_duplicate = final_df.columns.duplicated()
new_columns = np.where(is_duplicate, final_df.columns.astype(str) + "_" + counts.astype(str), final_df.columns)

# If there is some columns has only zeros, drop the column
# loc[:, ...] to filter through columns
# axis=0 means counting each column from up to down
final_df = final_df.loc[:, (final_df.sum(axis=0) != 0) | (final_df.columns == "risk_score")]

# Check variable types and full summary
final_df.info()

# Create feature is_high_risk for determining if the risk score is greater than or equal to 30% (high risk)
final_df["is_high_risk"] = (final_df["risk_score"] >= 0.3).astype("uint8")

high_risk_count = (final_df["risk_score"] >= 0.3).sum()
total_samples = len(final_df)
print(f"high_risk_count : {high_risk_count}")
print(f"total_samples : {total_samples}")
# Around 90% of the data are high risk (greater than or equal to 30%)

# 3. Model building

# Train / Validation / Test Split
X = final_df.drop(columns=["risk_score", "is_high_risk"])
y = final_df["is_high_risk"]
# Split 70% of the data for the training set, leaving 30% for further distribution
# random_state=42 is to set seed (to reproduce the same result)
# stratify=y is to guarantees there some low risk data across the splits
X_train, X_temp, y_train, y_temp = train_test_split(X, y, test_size=0.30, random_state=42, stratify=y)

# Equally split the remaining 30% (each gets 15% of the total) into validation and test sets
X_val, X_test, y_val, y_test = train_test_split(X_temp, y_temp, test_size=0.50, random_state=42, stratify=y_temp)

# Avoid any column name error when running the model
X_train_np = X_train.values
X_val_np = X_val.values
X_test_np = X_test.values

y_train_np = y_train.values
y_val_np = y_val.values
y_test_np = y_test.values

# Model: Gradient boosting

# Step1: Log Transformation for clearing the extreme values
X_train_log = np.log1p(X_train.values if hasattr(X_train, 'values') else X_train)
X_val_log = np.log1p(X_val.values if hasattr(X_val, 'values') else X_val)
X_test_log = np.log1p(X_test.values if hasattr(X_test, 'values') else X_test)

y_train_arr = y_train.values if hasattr(y_train, 'values') else y_train
y_val_arr = y_val.values if hasattr(y_val, 'values') else y_val
y_test_arr = y_test.values if hasattr(y_test, 'values') else y_test

# Step2: Train Gradient Boosting Tree with seed so it can regenerate the result
gbt_model = HistGradientBoostingClassifier(max_iter=400, learning_rate=0.03, max_leaf_nodes=127, max_depth=10, random_state=42)

# Auto calculate the sample weight and train
num_low_risk = np.sum(y_train_arr == 0)
num_high_risk = np.sum(y_train_arr == 1)
sample_weights = np.where(y_train_arr == 0, num_high_risk / num_low_risk, 1.0)
gbt_model.fit(X_train_log, y_train_arr, sample_weight=sample_weights)

# Find the threshold that recall and precision is higher than 90%
val_probs = gbt_model.predict_proba(X_val_log)[:, 1]
test_probs = gbt_model.predict_proba(X_test_log)[:, 1]

# find it between 0.35 to 0.55 is because the data is imbalance. Before using 0.5, using bias correction to increase recall
thresholds = np.linspace(0.35, 0.55, 21)
best_t = 0.50

for i in thresholds:
    current_preds = (val_probs >= i).astype(int)
    p1 = precision_score(y_val_arr, current_preds, pos_label=1, zero_division=0)
    r1 = recall_score(y_val_arr, current_preds, pos_label=1, zero_division=0)

    # If precision and recall in high risk are both above 90%, break
    if p1 >= 0.90 and r1 >= 0.90:
        best_t = i
        break

# Step4: Output Validation and Test Split Reports
final_val_preds = (val_probs >= best_t).astype(int)
print("Validation Split Report:")
print(
    classification_report(
        y_val_arr,
        final_val_preds,
        target_names=["Low Risk (0)", "High Risk (1)"],
        digits=4
    )
)

final_test_preds = (test_probs >= best_t).astype(int)
print("Test Split Report:")
print(
    classification_report(
        y_test_arr,
        final_test_preds,
        target_names=["Low Risk (0)", "High Risk (1)"],
        digits=4
    )
)

# Step5: Variable Importance Plot
result = permutation_importance(gbt_model, X_val_log, y_val_arr, n_repeats=3, random_state=42, n_jobs=-1)

feature_names = np.array(X_train.columns)
sorted_importances_idx = result.importances_mean.argsort()[::-1]

top_n = 15
plt.figure(figsize=(10, 6))
plt.title(f"Top {top_n} Most Powerful Malware Features (Variable Importance)")
plt.barh(
    range(top_n),
    result.importances_mean[sorted_importances_idx[:top_n]][::-1],
    align="center",
    color="blue"
)
plt.yticks(
    range(top_n), feature_names[sorted_importances_idx[:top_n]][::-1]
)
plt.xlabel("Mean Accuracy Decrease on Permutation")
plt.tight_layout()

# Save it to the same folder
plot_output_path = rawdata_folder / "feature_importance_plot.png"
plt.savefig(plot_output_path, dpi=300)

# Step6: Save the model to a pickle file
export_pipeline = {"final_model": gbt_model, "optimal_threshold": best_t}

model_output_path = rawdata_folder / "final_model.pkl"
with open(model_output_path, "wb") as f:
    pickle.dump(export_pipeline, f)
