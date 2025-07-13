// Install these packages first:
// npm install neo4j-driver

// pages/api/colleagues.js - API route
import { Neo4jDriver } from 'neo4j-driver';

export default async function handler(req, res) {
  const driver = Neo4jDriver.driver(
    process.env.NEO4J_URI,
    Neo4jDriver.auth.basic(process.env.NEO4J_USER, process.env.NEO4J_PASSWORD)
  );
  
  const session = driver.session();
  
  try {
    // Create graph structure similar to the example in the image
    if (req.method === 'POST') {
      await session.run(`
        CREATE (maria:Person {name: 'Maria'})
        CREATE (joao:Person {name: 'João'})
        CREATE (pedro:Person {name: 'Pedro'})
        CREATE (geografia:Topic {name: 'Geografia'})
        CREATE (estatistica:Topic {name: 'Estatística'})
        
        CREATE (maria)-[:COLEGA]->(joao)
        CREATE (joao)-[:COLEGA]->(pedro)
        CREATE (maria)-[:INTERESSADO_EM]->(geografia)
        CREATE (joao)-[:INTERESSADO_EM]->(geografia)
        CREATE (joao)-[:INTERESSADO_EM]->(estatistica)
        CREATE (pedro)-[:INTERESSADO_EM]->(estatistica)
      `);
      
      return res.status(200).json({ message: 'Graph data created' });
    }
    
    // Get colleagues and their interests
    if (req.method === 'GET') {
      const result = await session.run(`
        MATCH (p1:Person)-[:COLEGA]->(p2:Person)
        MATCH (p1)-[:INTERESSADO_EM]->(t:Topic)
        RETURN p1.name AS person, collect(p2.name) AS colleagues, collect(DISTINCT t.name) AS interests
      `);
      
      const data = result.records.map(record => ({
        person: record.get('person'),
        colleagues: record.get('colleagues'),
        interests: record.get('interests')
      }));
      
      return res.status(200).json(data);
    }
    
  } catch (error) {
    return res.status(500).json({ error: error.message });
  } finally {
    await session.close();
    await driver.close();
  }
}

// pages/index.js - Frontend component
import { useState, useEffect } from 'react';

export default function Home() {
  const [graphData, setGraphData] = useState([]);
  const [loading, setLoading] = useState(true);
  
  useEffect(() => {
    async function fetchData() {
      try {
        const response = await fetch('/api/colleagues');
        const data = await response.json();
        setGraphData(data);
      } catch (error) {
        console.error('Error fetching graph data:', error);
      } finally {
        setLoading(false);
      }
    }
    
    fetchData();
  }, []);
  
  return (
    <div className="container mx-auto p-4">
      <h1 className="text-2xl font-bold mb-4">Colegas e seus Interesses</h1>
      
      {loading ? (
        <p>Carregando...</p>
      ) : (
        <div className="grid grid-cols-1 gap-4">
          {graphData.map((person, index) => (
            <div key={index} className="border p-4 rounded shadow">
              <h2 className="text-xl font-semibold">{person.person}</h2>
              <div className="mt-2">
                <h3 className="font-medium">Colegas:</h3>
                <ul className="list-disc ml-6">
                  {person.colleagues.map((colleague, idx) => (
                    <li key={idx}>{colleague}</li>
                  ))}
                </ul>
              </div>
              <div className="mt-2">
                <h3 className="font-medium">Interesses:</h3>
                <ul className="list-disc ml-6">
                  {person.interests.map((interest, idx) => (
                    <li key={idx}>{interest}</li>
                  ))}
                </ul>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}