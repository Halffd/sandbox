// Graph representation of Southeast Brazil cities and distances
class Graph {
  constructor() {
    this.vertices = new Map();
    this.edges = [];
  }

  addVertex(city) {
    this.vertices.set(city, []);
    return this;
  }

  addEdge(city1, city2, distance) {
    // Add edge in both directions (undirected graph)
    if (this.vertices.has(city1) && this.vertices.has(city2)) {
      this.vertices.get(city1).push({ city: city2, distance });
      this.vertices.get(city2).push({ city: city1, distance });
      this.edges.push({ from: city1, to: city2, distance });
    }
    return this;
  }

  getNeighbors(city) {
    return this.vertices.get(city) || [];
  }

  getAllVertices() {
    return Array.from(this.vertices.keys());
  }

  // Implementação para o Problema do Caixeiro Viajante
  // Usando uma abordagem de força bruta para grafos pequenos
  findShortestTour(startCity) {
    const cities = this.getAllVertices();
    // Remover a cidade inicial da lista, pois ela será fixa
    const otherCities = cities.filter(city => city !== startCity);
    
    let shortestDistance = Infinity;
    let shortestPath = [];
    
    // Função para calcular a distância entre duas cidades
    const getDistance = (cityA, cityB) => {
      const neighbors = this.getNeighbors(cityA);
      const connection = neighbors.find(n => n.city === cityB);
      return connection ? connection.distance : Infinity;
    };
    
    // Função para calcular a distância total de um caminho
    const calculatePathDistance = (path) => {
      let totalDistance = 0;
      for (let i = 0; i < path.length - 1; i++) {
        const distance = getDistance(path[i], path[i + 1]);
        if (distance === Infinity) return Infinity;
        totalDistance += distance;
      }
      // Adicionar a distância de volta para a cidade inicial
      totalDistance += getDistance(path[path.length - 1], path[0]);
      return totalDistance;
    };
    
    // Gerar todas as permutações possíveis (para grafos pequenos)
    function generatePermutations(arr) {
      const result = [];
      
      // Função recursiva para gerar permutações
      function permute(arr, m = []) {
        if (arr.length === 0) {
          result.push(m);
        } else {
          for (let i = 0; i < arr.length; i++) {
            const curr = arr.slice();
            const next = curr.splice(i, 1);
            permute(curr, m.concat(next));
          }
        }
      }
      
      permute(arr);
      return result;
    }
    
    // Gerar todas as permutações possíveis das outras cidades
    const permutations = generatePermutations(otherCities);
    
    // Testar cada permutação
    for (const perm of permutations) {
      // Adicionar a cidade inicial no começo
      const path = [startCity, ...perm];
      
      const distance = calculatePathDistance(path);
      if (distance < shortestDistance) {
        shortestDistance = distance;
        shortestPath = path.slice(); // Criar uma cópia
      }
    }
    
    // Adicionar a cidade inicial ao final para completar o ciclo
    shortestPath.push(startCity);
    
    return {
      path: shortestPath,
      distance: shortestDistance
    };
  }
}

// Criar o grafo do Sudeste do Brasil
const brazilSEGraph = new Graph();

// Adicionar cidades (vértices)
brazilSEGraph
  .addVertex("São Paulo")
  .addVertex("Rio de Janeiro")
  .addVertex("Belo Horizonte")
  .addVertex("Vitória");

// Adicionar conexões com distâncias (arestas)
brazilSEGraph
  .addEdge("São Paulo", "Rio de Janeiro", 429)
  .addEdge("São Paulo", "Belo Horizonte", 586)
  .addEdge("São Paulo", "Vitória", 741)
  .addEdge("Rio de Janeiro", "Belo Horizonte", 339)
  .addEdge("Rio de Janeiro", "Vitória", 412)
  .addEdge("Belo Horizonte", "Vitória", 378);

// Encontrar o menor trajeto partindo de São Paulo
const shortestTour = brazilSEGraph.findShortestTour("São Paulo");

console.log("Menor trajeto para percorrer todas as capitais partindo de São Paulo:");
console.log(`Rota: ${shortestTour.path.join(' → ')}`);
console.log(`Distância total: ${shortestTour.distance} km`);

// Mostrar o trajeto detalhado
console.log("\nTrajeto detalhado:");
for (let i = 0; i < shortestTour.path.length - 1; i++) {
  const from = shortestTour.path[i];
  const to = shortestTour.path[i + 1];
  
  const distance = brazilSEGraph.getNeighbors(from)
    .find(n => n.city === to).distance;
  
  console.log(`${from} → ${to}: ${distance} km`);
}
