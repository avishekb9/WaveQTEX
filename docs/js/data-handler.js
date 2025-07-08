// Data Handler for WaveQTE Network Economics
// Manages data collection, processing, and real-time updates

class DataHandler {
    constructor() {
        this.marketData = new Map();
        this.networkData = new Map();
        this.agentData = new Map();
        this.riskData = new Map();
        
        this.updateInterval = null;
        this.isConnected = false;
        this.retryCount = 0;
        this.maxRetries = 5;
        
        this.init();
    }
    
    init() {
        this.setupMarketData();
        this.setupNetworkData();
        this.setupAgentData();
        this.setupRiskData();
        
        this.startDataUpdates();
        this.setupEventListeners();
    }
    
    setupMarketData() {
        // Initialize market data structure
        const markets = [
            'US_SP500', 'US_NASDAQ', 'US_RUSSELL',
            'EU_STOXX', 'DE_DAX', 'FR_CAC40', 'UK_FTSE',
            'JP_NIKKEI', 'JP_TOPIX', 'AU_ASX', 'CA_TSX', 'CH_SMI',
            'CN_SHANGHAI', 'CN_CSI300', 'HK_HANGSENG',
            'IN_SENSEX', 'IN_NIFTY', 'BR_BOVESPA', 'ZA_JSE',
            'RU_MOEX', 'KR_KOSPI', 'TW_TAIEX', 'MX_IPC', 'TR_BIST'
        ];
        
        markets.forEach(market => {
            this.marketData.set(market, {
                name: market,
                price: 1000 + Math.random() * 4000,
                change: (Math.random() - 0.5) * 0.1,
                volume: Math.random() * 1000000,
                volatility: Math.random() * 0.5,
                returns: this.generateReturns(252), // 1 year of daily returns
                correlations: new Map(),
                lastUpdate: new Date()
            });
        });
        
        // Generate correlation matrix
        this.generateCorrelationMatrix();
    }
    
    setupNetworkData() {
        // Initialize network structure data
        const scales = [1, 2, 3, 4, 5, 6];
        
        scales.forEach(scale => {
            this.networkData.set(scale, {
                scale: scale,
                nodes: this.generateNetworkNodes(),
                edges: this.generateNetworkEdges(),
                density: Math.random() * 0.5 + 0.3,
                clustering: Math.random() * 0.6 + 0.2,
                centrality: this.generateCentralityMeasures(),
                lastUpdate: new Date()
            });
        });
    }
    
    setupAgentData() {
        // Initialize agent data
        const agentTypes = ['hft', 'market_maker', 'institutional', 'pension', 'regulator'];
        
        agentTypes.forEach(type => {
            const agentCount = this.getAgentCount(type);
            const agents = [];
            
            for (let i = 0; i < agentCount; i++) {
                agents.push({
                    id: `${type.toUpperCase()}-${String(i + 1).padStart(3, '0')}`,
                    type: type,
                    status: 'active',
                    performance: Math.random() * 0.3 + 0.7,
                    latency: Math.random() * 2 + 0.5,
                    messages: Math.floor(Math.random() * 1000 + 100),
                    profit: Math.random() * 50000,
                    riskAversion: Math.random() * 0.5 + 0.3,
                    portfolio: this.generatePortfolio(),
                    lastUpdate: new Date()
                });
            }
            
            this.agentData.set(type, agents);
        });
    }
    
    setupRiskData() {
        // Initialize risk data
        this.riskData.set('components', {
            market: Math.random() * 0.5 + 0.2,
            credit: Math.random() * 0.4 + 0.15,
            liquidity: Math.random() * 0.6 + 0.25,
            operational: Math.random() * 0.3 + 0.1
        });
        
        this.riskData.set('indicators', [
            { name: 'VIX', value: Math.random() * 20 + 15, threshold: 25 },
            { name: 'Credit Spreads', value: Math.random() * 50 + 100, threshold: 150 },
            { name: 'Liquidity Ratio', value: Math.random() * 0.3 + 0.7, threshold: 0.5 },
            { name: 'Correlation', value: Math.random() * 0.4 + 0.5, threshold: 0.7 }
        ]);
        
        this.riskData.set('history', this.generateRiskHistory());
    }
    
    generateReturns(count) {
        const returns = [];
        for (let i = 0; i < count; i++) {
            returns.push((Math.random() - 0.5) * 0.05);
        }
        return returns;
    }
    
    generateCorrelationMatrix() {
        const markets = Array.from(this.marketData.keys());
        
        markets.forEach(market1 => {
            markets.forEach(market2 => {
                if (market1 !== market2) {
                    let correlation = Math.random() * 0.8 + 0.1;
                    
                    // Higher correlation for same region
                    if (this.sameRegion(market1, market2)) {
                        correlation = Math.max(correlation, Math.random() * 0.4 + 0.5);
                    }
                    
                    this.marketData.get(market1).correlations.set(market2, correlation);
                }
            });
        });
    }
    
    sameRegion(market1, market2) {
        const regions = {
            'US': ['US_SP500', 'US_NASDAQ', 'US_RUSSELL'],
            'EU': ['EU_STOXX', 'DE_DAX', 'FR_CAC40', 'UK_FTSE'],
            'ASIA': ['JP_NIKKEI', 'JP_TOPIX', 'CN_SHANGHAI', 'CN_CSI300', 'HK_HANGSENG', 'IN_SENSEX', 'IN_NIFTY']
        };
        
        for (const region of Object.values(regions)) {
            if (region.includes(market1) && region.includes(market2)) {
                return true;
            }
        }
        
        return false;
    }
    
    generateNetworkNodes() {
        const nodes = [];
        const marketNames = Array.from(this.marketData.keys());
        
        marketNames.forEach((market, index) => {
            nodes.push({
                id: index,
                name: market,
                type: market.includes('US') || market.includes('EU') || market.includes('JP') ? 'developed' : 'emerging',
                centrality: Math.random(),
                degree: Math.floor(Math.random() * 10 + 5),
                position: {
                    x: Math.random() * 100,
                    y: Math.random() * 100,
                    z: Math.random() * 100
                }
            });
        });
        
        return nodes;
    }
    
    generateNetworkEdges() {
        const edges = [];
        const nodeCount = this.marketData.size;
        
        for (let i = 0; i < nodeCount; i++) {
            for (let j = i + 1; j < nodeCount; j++) {
                if (Math.random() < 0.3) {
                    edges.push({
                        source: i,
                        target: j,
                        weight: Math.random(),
                        type: 'transfer_entropy'
                    });
                }
            }
        }
        
        return edges;
    }
    
    generateCentralityMeasures() {
        const markets = Array.from(this.marketData.keys());
        const centrality = new Map();
        
        markets.forEach(market => {
            centrality.set(market, {
                degree: Math.random(),
                betweenness: Math.random(),
                closeness: Math.random(),
                eigenvector: Math.random()
            });
        });
        
        return centrality;
    }
    
    getAgentCount(type) {
        const counts = {
            'hft': 50,
            'market_maker': 20,
            'institutional': 30,
            'pension': 15,
            'regulator': 5
        };
        return counts[type] || 10;
    }
    
    generatePortfolio() {
        const portfolio = new Map();
        const markets = Array.from(this.marketData.keys());
        
        // Select random subset of markets
        const portfolioSize = Math.floor(Math.random() * 5 + 3);
        const selectedMarkets = markets.sort(() => 0.5 - Math.random()).slice(0, portfolioSize);
        
        let totalWeight = 0;
        selectedMarkets.forEach(market => {
            const weight = Math.random();
            portfolio.set(market, weight);
            totalWeight += weight;
        });
        
        // Normalize weights
        portfolio.forEach((weight, market) => {
            portfolio.set(market, weight / totalWeight);
        });
        
        return portfolio;
    }
    
    generateRiskHistory() {
        const history = [];
        const days = 90;
        
        for (let i = 0; i < days; i++) {
            const date = new Date();
            date.setDate(date.getDate() - (days - i));
            
            history.push({
                date: date,
                aggregate: Math.random() * 0.6 + 0.2,
                market: Math.random() * 0.5 + 0.2,
                credit: Math.random() * 0.4 + 0.15,
                liquidity: Math.random() * 0.6 + 0.25,
                operational: Math.random() * 0.3 + 0.1
            });
        }
        
        return history;
    }
    
    startDataUpdates() {
        this.updateInterval = setInterval(() => {
            this.updateMarketData();
            this.updateNetworkData();
            this.updateAgentData();
            this.updateRiskData();
            
            this.broadcastUpdate();
        }, 2000); // Update every 2 seconds
    }
    
    updateMarketData() {
        this.marketData.forEach((data, market) => {
            // Update price with random walk
            const change = (Math.random() - 0.5) * 0.02;
            data.price = Math.max(100, data.price * (1 + change));
            data.change = change;
            
            // Update volume
            data.volume = Math.max(0, data.volume * (1 + (Math.random() - 0.5) * 0.3));
            
            // Update volatility
            data.volatility = Math.max(0, data.volatility + (Math.random() - 0.5) * 0.05);
            
            // Add new return
            data.returns.push(change);
            if (data.returns.length > 252) {
                data.returns.shift();
            }
            
            data.lastUpdate = new Date();
        });
        
        // Update correlations occasionally
        if (Math.random() < 0.1) {
            this.generateCorrelationMatrix();
        }
    }
    
    updateNetworkData() {
        this.networkData.forEach((data, scale) => {
            // Update network density
            data.density = Math.max(0, Math.min(1, data.density + (Math.random() - 0.5) * 0.1));
            
            // Update clustering coefficient
            data.clustering = Math.max(0, Math.min(1, data.clustering + (Math.random() - 0.5) * 0.05));
            
            // Update node centralities
            data.centrality.forEach((centrality, market) => {
                Object.keys(centrality).forEach(measure => {
                    centrality[measure] = Math.max(0, Math.min(1, centrality[measure] + (Math.random() - 0.5) * 0.05));
                });
            });
            
            // Update edge weights
            data.edges.forEach(edge => {
                edge.weight = Math.max(0, Math.min(1, edge.weight + (Math.random() - 0.5) * 0.1));
            });
            
            data.lastUpdate = new Date();
        });
    }
    
    updateAgentData() {
        this.agentData.forEach((agents, type) => {
            agents.forEach(agent => {
                // Update performance
                agent.performance = Math.max(0, Math.min(1, agent.performance + (Math.random() - 0.5) * 0.01));
                
                // Update latency
                agent.latency = Math.max(0.1, agent.latency + (Math.random() - 0.5) * 0.1);
                
                // Update message count
                agent.messages = Math.max(0, agent.messages + Math.floor(Math.random() * 20 - 10));
                
                // Update profit (except for regulators)
                if (agent.type !== 'regulator') {
                    agent.profit += Math.random() * 1000 - 500;
                }
                
                // Occasionally update portfolio
                if (Math.random() < 0.05) {
                    agent.portfolio = this.generatePortfolio();
                }
                
                agent.lastUpdate = new Date();
            });
        });
    }
    
    updateRiskData() {
        // Update risk components
        const components = this.riskData.get('components');
        Object.keys(components).forEach(component => {
            components[component] = Math.max(0, Math.min(1, components[component] + (Math.random() - 0.5) * 0.05));
        });
        
        // Update risk indicators
        const indicators = this.riskData.get('indicators');
        indicators.forEach(indicator => {
            indicator.value = Math.max(0, indicator.value + (Math.random() - 0.5) * indicator.value * 0.1);
        });
        
        // Add new data point to history
        const history = this.riskData.get('history');
        history.push({
            date: new Date(),
            aggregate: Object.values(components).reduce((sum, val) => sum + val, 0) / Object.keys(components).length,
            ...components
        });
        
        if (history.length > 90) {
            history.shift();
        }
    }
    
    broadcastUpdate() {
        // Emit custom event for other components to listen to
        const updateEvent = new CustomEvent('dataUpdate', {
            detail: {
                timestamp: new Date(),
                markets: this.getMarketSummary(),
                network: this.getNetworkSummary(),
                agents: this.getAgentSummary(),
                risk: this.getRiskSummary()
            }
        });
        
        document.dispatchEvent(updateEvent);
    }
    
    // Data access methods
    getMarketData(market) {
        return this.marketData.get(market);
    }
    
    getAllMarketData() {
        return Array.from(this.marketData.values());
    }
    
    getNetworkData(scale) {
        return this.networkData.get(scale);
    }
    
    getAllNetworkData() {
        return Array.from(this.networkData.values());
    }
    
    getAgentData(type) {
        return this.agentData.get(type);
    }
    
    getAllAgentData() {
        const allAgents = [];
        this.agentData.forEach(agents => {
            allAgents.push(...agents);
        });
        return allAgents;
    }
    
    getRiskData() {
        return {
            components: this.riskData.get('components'),
            indicators: this.riskData.get('indicators'),
            history: this.riskData.get('history')
        };
    }
    
    // Summary methods
    getMarketSummary() {
        const markets = Array.from(this.marketData.values());
        return {
            count: markets.length,
            avgChange: markets.reduce((sum, m) => sum + m.change, 0) / markets.length,
            avgVolatility: markets.reduce((sum, m) => sum + m.volatility, 0) / markets.length,
            totalVolume: markets.reduce((sum, m) => sum + m.volume, 0)
        };
    }
    
    getNetworkSummary() {
        const networks = Array.from(this.networkData.values());
        return {
            scales: networks.length,
            avgDensity: networks.reduce((sum, n) => sum + n.density, 0) / networks.length,
            avgClustering: networks.reduce((sum, n) => sum + n.clustering, 0) / networks.length,
            totalEdges: networks.reduce((sum, n) => sum + n.edges.length, 0)
        };
    }
    
    getAgentSummary() {
        const allAgents = this.getAllAgentData();
        return {
            total: allAgents.length,
            active: allAgents.filter(a => a.status === 'active').length,
            avgPerformance: allAgents.reduce((sum, a) => sum + a.performance, 0) / allAgents.length,
            avgLatency: allAgents.reduce((sum, a) => sum + a.latency, 0) / allAgents.length,
            totalMessages: allAgents.reduce((sum, a) => sum + a.messages, 0)
        };
    }
    
    getRiskSummary() {
        const components = this.riskData.get('components');
        const indicators = this.riskData.get('indicators');
        
        return {
            aggregateRisk: Object.values(components).reduce((sum, val) => sum + val, 0) / Object.keys(components).length,
            components: components,
            criticalIndicators: indicators.filter(i => i.value > i.threshold).length,
            totalIndicators: indicators.length
        };
    }
    
    // Utility methods
    exportData() {
        return {
            timestamp: new Date(),
            markets: Object.fromEntries(this.marketData),
            network: Object.fromEntries(this.networkData),
            agents: Object.fromEntries(this.agentData),
            risk: Object.fromEntries(this.riskData)
        };
    }
    
    importData(data) {
        if (data.markets) {
            this.marketData = new Map(Object.entries(data.markets));
        }
        if (data.network) {
            this.networkData = new Map(Object.entries(data.network));
        }
        if (data.agents) {
            this.agentData = new Map(Object.entries(data.agents));
        }
        if (data.risk) {
            this.riskData = new Map(Object.entries(data.risk));
        }
    }
    
    setupEventListeners() {
        // Listen for page visibility changes
        document.addEventListener('visibilitychange', () => {
            if (document.hidden) {
                this.pauseUpdates();
            } else {
                this.resumeUpdates();
            }
        });
        
        // Listen for network status changes
        window.addEventListener('online', () => {
            this.isConnected = true;
            this.retryCount = 0;
            this.resumeUpdates();
        });
        
        window.addEventListener('offline', () => {
            this.isConnected = false;
            this.pauseUpdates();
        });
    }
    
    pauseUpdates() {
        if (this.updateInterval) {
            clearInterval(this.updateInterval);
            this.updateInterval = null;
        }
    }
    
    resumeUpdates() {
        if (!this.updateInterval) {
            this.startDataUpdates();
        }
    }
    
    destroy() {
        this.pauseUpdates();
        this.marketData.clear();
        this.networkData.clear();
        this.agentData.clear();
        this.riskData.clear();
    }
}

// WebSocket connection simulation
class WebSocketSimulator {
    constructor() {
        this.isConnected = false;
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 5;
        this.reconnectDelay = 1000;
        
        this.messageQueue = [];
        this.subscribers = new Map();
        
        this.connect();
    }
    
    connect() {
        console.log('Simulating WebSocket connection...');
        
        // Simulate connection delay
        setTimeout(() => {
            this.isConnected = true;
            this.reconnectAttempts = 0;
            console.log('WebSocket connected (simulated)');
            
            this.emit('connect');
            this.startHeartbeat();
        }, 1000);
    }
    
    disconnect() {
        this.isConnected = false;
        console.log('WebSocket disconnected');
        this.emit('disconnect');
    }
    
    reconnect() {
        if (this.reconnectAttempts < this.maxReconnectAttempts) {
            this.reconnectAttempts++;
            
            setTimeout(() => {
                this.connect();
            }, this.reconnectDelay * this.reconnectAttempts);
        } else {
            console.error('Max reconnection attempts reached');
            this.emit('error', 'Max reconnection attempts reached');
        }
    }
    
    send(message) {
        if (this.isConnected) {
            console.log('Sending message:', message);
            
            // Simulate message echo
            setTimeout(() => {
                this.emit('message', {
                    type: 'echo',
                    data: message,
                    timestamp: new Date()
                });
            }, 100);
        } else {
            this.messageQueue.push(message);
        }
    }
    
    subscribe(channel, callback) {
        if (!this.subscribers.has(channel)) {
            this.subscribers.set(channel, new Set());
        }
        this.subscribers.get(channel).add(callback);
    }
    
    unsubscribe(channel, callback) {
        if (this.subscribers.has(channel)) {
            this.subscribers.get(channel).delete(callback);
        }
    }
    
    emit(event, data) {
        if (this.subscribers.has(event)) {
            this.subscribers.get(event).forEach(callback => {
                callback(data);
            });
        }
    }
    
    startHeartbeat() {
        setInterval(() => {
            if (this.isConnected) {
                this.emit('heartbeat', { timestamp: new Date() });
            }
        }, 30000); // Every 30 seconds
    }
}

// Initialize data systems
let dataHandler = null;
let webSocketSimulator = null;

document.addEventListener('DOMContentLoaded', () => {
    dataHandler = new DataHandler();
    webSocketSimulator = new WebSocketSimulator();
    
    // Make available globally
    window.dataHandler = dataHandler;
    window.webSocketSimulator = webSocketSimulator;
    
    // Set up data update listeners
    document.addEventListener('dataUpdate', (event) => {
        // console.log('Data update received:', event.detail);
        
        // Update UI elements that depend on data
        updateDataDependentElements(event.detail);
    });
});

function updateDataDependentElements(data) {
    // Update network statistics
    const nodeCount = document.getElementById('node-count');
    const edgeCount = document.getElementById('edge-count');
    const networkDensity = document.getElementById('network-density');
    
    if (nodeCount) nodeCount.textContent = data.network.scales * 24;
    if (edgeCount) edgeCount.textContent = data.network.totalEdges;
    if (networkDensity) networkDensity.textContent = data.network.avgDensity.toFixed(2);
    
    // Update agent statistics
    const agentCount = document.getElementById('agents-value');
    if (agentCount) agentCount.textContent = data.agents.total.toLocaleString();
    
    // Update risk metrics
    const sriValue = document.getElementById('sri-value');
    if (sriValue) sriValue.textContent = data.risk.aggregateRisk.toFixed(3);
}

// Export classes
window.DataHandler = DataHandler;
window.WebSocketSimulator = WebSocketSimulator;