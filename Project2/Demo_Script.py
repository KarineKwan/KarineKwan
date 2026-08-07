import pickle
import pandas as pd
import numpy as np
from pathlib import Path


def main():
    # locate the package model
    current_dir = Path(__file__).resolve().parent
    model_path = current_dir / "final_model.pkl"

    if not model_path.exists():
        raise FileNotFoundError(
            f"Please make sure you have the final_model.pkl in your folder."
        )

    with open(model_path, "rb") as f:
        payload = pickle.load(f)

    gbt_model = payload["final_model"]
    optimized_threshold = payload["optimal_threshold"]
    trained_features = payload["feature_names"]

    # Use first five columns in the dataset to test the result
    # demo_x and demo_y is saved in main.py as demo
    raw_input_data = payload["demo_x"]
    ground_truth_y = payload["demo_y"].values

    print(f"show the raw data")
    print(raw_input_data)

    # Make sure the column is aligned with the package model
    aligned_data = raw_input_data.reindex(columns=trained_features, fill_value=np.nan)

    aligned_data_np = aligned_data.values
    aligned_data_log = np.log1p(aligned_data_np)

    # Calculate the high risk probability
    probabilities = gbt_model.predict_proba(aligned_data_log)[:, 1]
    # Apply the optimal threshold from the packaged model
    predictions = (probabilities >= optimized_threshold).astype(int)

    print("Model Output:")
    for i, (pred, prob, real_y) in enumerate(zip(predictions, probabilities, ground_truth_y)):
        pred_status = "【High Risk】" if pred == 1 else "【Low Risk】"
        real_status = "High Risk" if real_y == 1 else "Low Risk"

        # Check with real status to see if the prediction is correct
        is_correct = "Predicted Correctly" if pred == real_y else "Predicted Wrong"

        print(
            f"Sample Data {i + 1} -> Predict high risk probability: {prob:7.2%} | Predict: {pred} {pred_status} | Real Status: {real_y} ({real_status}) | {is_correct}")

    print(f"The optimized threshold is {optimized_threshold:.4f}")


if __name__ == "__main__":
    main()