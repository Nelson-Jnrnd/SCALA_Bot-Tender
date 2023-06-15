# SCALA_Bot-Tender

## Description
On a décidé d'implémenter les fonctionnalitées asynchrones dans AnalyzerService pour dissocier la logique des services de l'interface WEB.

Pour pouvoir ajouter les messages au chat on a utilisé un système de callback. Les utilisateurs de la classe AnalyzerService peuvent utiliser la méthode setCallback pour définir la fonction qui sera appelée à chaque fois qu'une commande est prête.

Ici, MessagesRoutes crée un callback qui va ajouter le message au chat et le mettre à jour.

## AnalyzerService

On a modifié le case Order dans la méthode reply pour prendre en compte le temps de préparation via la méthode computeFuturePrice.

### computeFuturePrice
Cette méthode va retourner une List[Future[Double]] qui représente le prix de chaque produits. Elle est récursive et traite les produits avec les méthodes prepareProduct qui va elle chainer les futures pour calculer le prix d'un produit (selon la quantité)

## Gestion des Failures
La méthode computeFuturePrice ne retourne que des success. Si une préparation échoue, son prix ne sera pas considéré. Donc pour détecter si une commande est incomplète on va comparer le prix avec celui calculé par la méthode computePrice.
