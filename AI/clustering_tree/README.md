Voici le README mis à jour avec l'indication de la licence MIT :

---

# Geographic Cluster Prediction

This Python script `clustering_trees` implements a geographic cluster prediction program using a pre-trained K-Means model.

## Table of Contents
1. [Imports](#imports)
2. [Functions](#functions)
3. [Main Function](#main-function)
4. [Code Execution](#code-execution)
5. [Dependencies](#dependencies)
6. [Examples of Execution](#examples-of-execution)
7. [License](#license)

## Imports
```bash
pip install -r requirements.txt
```

## Functions
- **`checkArguments()`**: Checks and retrieves the command-line arguments provided.
- **`load_model(file_path)`**: Loads the K-Means model from a specified file.
- **`load_scaler(file_path)`**: Loads the scaler from a specified file.
- **`predict_cluster(model, scaler, args)`**: Predicts the cluster for the new data provided in the arguments.
- **`plot_cluster_map(data)`**: Displays existing data with the predicted clusters on an interactive map.

## Main Function
The main function `main()`:
- Checks the arguments.
- Loads the appropriate K-Means model and scaler.
- Reads data from a CSV file, predicts clusters, and visualizes the results.

## Code Execution
To execute this script, follow these steps:

1. **Generate the necessary files**:
   - Ensure you have generated the files `kmeans.pkl`, `kmeans2.pkl`, `label_encoder.pkl`, and `scaler.pkl` by running the associated notebook.

2. **Prepare the required files**:
   - Make sure the file `Data_Arbre.csv` is in the same directory as the script.

3. **Install dependencies**:
   ```bash
   pip install pandas scikit-learn plotly joblib
   ```

4. **Run the script from the command line**:
   ```bash
   python script_client1.py --latitude 48.8566 --longitude 2.3522 --haut_tot 15 --k 2
   ```

### Examples of Execution
Here is an example of execution:
```bash
python clustering_trees.py --latitude 48.8566 --longitude 2.3522 --haut_tot 15 --k 2
```
This command uses the geographic coordinates of Paris, a tree height of 15 meters, and the K-Means model with 2 clusters.

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

