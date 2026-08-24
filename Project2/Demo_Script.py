import pickle
import pandas as pd
import numpy as np
from pathlib import Path


def main():
    # locate the package model
    current_dir = Path(__file__).resolve().parent
    model_path = current_dir / "final_model.pkl"
    input_csv_path = current_dir / "demo_inputs.csv"

    if not model_path.exists():
        raise FileNotFoundError(f"Please make sure you have the final_model.pkl in your folder.")
    if not input_csv_path.exists():
        raise FileNotFoundError(f"Please make sure you have the demo_inputs.csv in your folder.")

    with open(model_path, "rb") as f:
        payload = pickle.load(f)

    gbt_model = payload["final_model"]
    optimized_threshold = payload["optimal_threshold"]
    trained_features = payload["feature_names"]

    # if there is no DEMO_Y_RESULT value or no column, put it as -1 for showing it is unknown
    external_df = pd.read_csv(input_csv_path)
    if "DEMO_Y_RESULT" in external_df.columns:
        ground_truth_y = external_df["DEMO_Y_RESULT"].values
        raw_features_df = external_df.drop(columns=["DEMO_Y_RESULT"])
    else:
        ground_truth_y = np.array([-1] * len(external_df))
        raw_features_df = external_df

    print(f"show the raw data")
    print(raw_features_df)

    aligned_data = raw_features_df.reindex(columns=trained_features, fill_value=np.nan)

    aligned_data_np = aligned_data.values.astype(np.float32)
    aligned_data_log = np.log1p(aligned_data_np)

    probabilities = gbt_model.predict_proba(aligned_data_log)[:, 1]
    predictions = (probabilities >= optimized_threshold).astype(int)

    print("Model Output:")
    for i, (pred, prob, real_y) in enumerate(zip(predictions, probabilities, ground_truth_y)):
        pred_status = "[High Risk]" if pred == 1 else "[Low Risk]"

        if real_y != -1:
            real_status = "High Risk" if real_y == 1 else "Low Risk"
            is_correct = "Predicted Correctly" if pred == real_y else "Predicted Wrong"
            label_evidence = f"| Real Status: {real_y} ({real_status}) | {is_correct}"
        else:
            label_evidence = "| Real Status: [Unknown/User-Edited Sample Data]"

        print(
            f"Sample Row {i + 1} -> Predict high risk probability: {prob:7.2%} | Predict: {pred} {pred_status:11} {label_evidence}")
    print(f"The optimized threshold is {optimized_threshold:.4f}")


if __name__ == "__main__":
    main()