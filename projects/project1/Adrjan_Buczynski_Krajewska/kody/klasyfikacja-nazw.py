# %%
from main import parse_data
import numpy as np
import pandas as pd
import os
from matplotlib import pyplot as plt
# %%
sql, _ = parse_data()
# %%
names = pd.read_sql_query(
    """
    SELECT nazwa, COUNT(*) AS count
    FROM ulic
    GROUP BY nazwa
    ORDER BY nazwa;
    """, sql
)
# %%
names.shape
# %%
batch_size = 1000
randomized_names = names.sample(frac=1, random_state=42).reset_index(drop=True)
for start in range(0, len(randomized_names), batch_size):
    end = start + batch_size
    batch = randomized_names.nazwa[start:end].sort_values().tolist()
    with open(f"data/classification-randomized/input/{start//batch_size + 1}.txt", "w") as f:
        f.write("\n".join(batch))
# %%
# use an LLM to classify batched names
# prompt: data/classification-randomized/prompt.txt
# input: data/classification-randomized/input/xx.txt
# output: data/classification-randomized/output/xx.csv
# %%
classified = pd.read_csv(f"data/classification-randomized/output/01.csv", header=None, names=["nazwa", "kategoria"], sep=";").set_index("nazwa")
# %%
classified = pd.concat([
    pd.read_csv(f"data/classification-randomized/output/{file}", header=None, names=["nazwa", "kategoria"], sep=";").set_index("nazwa")
    for file in os.listdir("data/classification-randomized/output/") if file.endswith(".csv")
])
# %%
joined = names.join(
    classified,
    on="nazwa",
    how="right",
)
# %%
joined.head(20)
# %%
summary = joined.groupby("kategoria").sum(numeric_only=True)
plt.pie(summary["count"], labels=summary.index, autopct="%1.1f%%")
# %%
most_common_by_category = joined.groupby("kategoria").apply(lambda df: df.sort_values("count", ascending=False).head(10)).reset_index(drop=True)
most_common_by_category
# %%
