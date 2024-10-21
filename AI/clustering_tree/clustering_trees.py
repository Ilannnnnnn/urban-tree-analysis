import argparse
import joblib
import pandas as pd
from sklearn.preprocessing import StandardScaler
import plotly.express as px

def checkArguments():
    """Vérifie les arguments du programme et retourne les paramètres du programme."""
    parser = argparse.ArgumentParser()
    parser.add_argument('-lat', '--latitude', type=float, required=True, help='latitude')
    parser.add_argument('-lon', '--longitude', type=float, required=True, help='longitude')
    parser.add_argument('-htot', '--haut_tot', type=float, required=True, help='hauteur totale')
    parser.add_argument('-k', '--clusters', type=int, required=True, help='nombre de clusters (K)')
    return parser.parse_args()

def load_model(file_path):
    """Charge le modèle K-Means à partir du fichier spécifié."""
    try:
        with open(file_path, 'rb') as file:
            model = joblib.load(file)
        return model
    except FileNotFoundError:
        print(f"Erreur : Le fichier '{file_path}' n'a pas été trouvé.")
        return None

def load_scaler(file_path):
    """Charge le scaler à partir du fichier spécifié."""
    try:
        with open(file_path, 'rb') as file:
            scaler = joblib.load(file)
        return scaler
    except FileNotFoundError:
        print(f"Erreur : Le fichier '{file_path}' n'a pas été trouvé.")
        return None

def predict_cluster(model, scaler, args):
    """Prédit le cluster pour les nouvelles données."""
    new_data = pd.DataFrame({
        'latitude': [args.latitude],
        'longitude': [args.longitude],
        'haut_tot': [args.haut_tot]
    })

    features = ['haut_tot']
    new_data_scaled = scaler.transform(new_data[features])

    cluster = model.predict(new_data_scaled)[0]
    return cluster, new_data

def main():
    args = checkArguments()

    if args.clusters == 2:
        model_path = 'kmeans2.pkl'
    elif args.clusters == 3:
        model_path = 'kmeans.pkl'
    else:
        print("Nombre de clusters non pris en charge. Choisissez 2 ou 3.")
        return

    scaler_path = 'scaler.pkl'
    
    print(f"Chargement du modèle depuis '{model_path}'...")
    kmeans_model = load_model(model_path)
    print(f"Chargement du scaler depuis '{scaler_path}'...")
    scaler = load_scaler(scaler_path)

    if kmeans_model and scaler:
        print("Modèle et scaler chargés avec succès.")

        # Charger les données complètes
        data_path = 'Data_Arbre.csv'
        print(f"Chargement des données depuis '{data_path}'...")
        data = pd.read_csv(data_path)
        print(f"Dimensions des données chargées : {data.shape}")
        
        data = data[['latitude', 'longitude', 'haut_tot']].dropna()

        # Prédire les clusters pour toutes les données existantes
        features = ['haut_tot']
        data_scaled = scaler.transform(data[features])
        clusters = kmeans_model.predict(data_scaled)
        data['cluster'] = clusters

        print("Prédiction des clusters pour les données existantes :")
        print(data.head())

        # Prédire le cluster pour les nouvelles données
        cluster, new_data = predict_cluster(kmeans_model, scaler, args)
        print(f"Le cluster prédit pour les nouvelles données est : {cluster}")

        # Ajouter le nouveau point aux données existantes
        new_data['cluster'] = cluster
        data = pd.concat([data, new_data], ignore_index=True)

        print("Données finales avec nouveau point ajouté :")
        print(data.tail())

    else:
        print("Impossible de charger le modèle ou le scaler. Vérifiez le chemin du fichier.")

if __name__ == "__main__":
    main()
