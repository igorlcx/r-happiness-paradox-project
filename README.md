# Analyse du « paradoxe du bonheur » en France

Ce projet analyse le « paradoxe du bonheur » en France à partir de statistiques descriptives en **R**.  
Il examine dans quelle mesure le revenu par habitant influence réellement la satisfaction de vie, en intégrant des facteurs comme la santé, l’aisance financière ressentie et les liens sociaux.  
Le code R et le rapport PDF inclus reproduisent les analyses univariées, multivariées et européennes sur les données de l’European Social Survey (ESS).

## Contenu clé

- Étude univariée : lien faible entre déciles de revenu et bonheur (coefficient de Spearman ρ ≈ 0,30).  
- Régression multivariée : revenu non significatif (p = 0,69), tandis que santé, aisance et vie sociale jouent un rôle central.
- Comparaison européenne : effet du revenu atténué dans les pays les plus riches, avec une heatmap des facteurs explicatifs par pays.

## Technologies

- Script R principal `DM-VF.R` utilisant `dplyr`, `ggplot2` et `broom` pour le nettoyage des données, les visualisations et l’estimation des modèles.  
- Construction d’un indicateur objectif de bonheur (`Mean410`) à partir des variables de l’ESS.

## Utilisation

- Exécuter le script R avec les données **ESS9** pour reproduire les graphiques et tableaux.  
