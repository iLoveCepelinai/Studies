# Python program for calculations in VM

import pandas as pd
import numpy as np

# reading the CSV file
csvFile = pd.read_csv('houzz_final_data_upd.csv')
csvFile['price'] = csvFile['price'].str.replace('[^0-9\.]', '', regex=True).str.strip()

 
from sentence_transformers import SentenceTransformer, util
model = SentenceTransformer('all-MiniLM-L6-v2')


embeddings_new = np.load("/content/gdrive/MyDrive/Studijos/4 kursas/Naturalios kalbos apdorojimas/embeddings.npy")

def getSimilarSentence(request_sentence, df, df_embedded):
  # Embed the model
  embedding = model.encode(request_sentence, convert_to_tensor=False)
  # Get cosine similarity scores (array)
  cosine_scores = util.cos_sim(embedding, df_embedded)

  # Assign to new column in og dataframe (if it doesn't work try deleting cpu() since I was using gpu for this)
  df['cosine scores'] = cosine_scores.cpu().numpy()[0]

  top_matches = df.sort_values("cosine scores", ascending=False).head(1)
  return(top_matches)