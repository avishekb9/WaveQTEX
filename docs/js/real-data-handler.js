// Real Data Handler for WaveQTE Network Economics
// Loads actual data from GitHub Actions generated JSON files

class RealDataHandler {
    constructor() {
        this.baseUrl = window.location.origin + window.location.pathname;
        this.dataCache = new Map();
        this.lastUpdateTime = null;
        this.updateInterval = null;
        this.retryCount = 0;
        this.maxRetries = 3;
        
        this.init();
    }
    
    init() {
        console.log('Initializing Real Data Handler...');
        this.loadAllData();
        this.startPeriodicUpdates();
        this.setupEventListeners();
    }
    
    async loadAllData() {
        try {
            const dataFiles = [
                'market_data.json',
                'network_data.json', 
                'agent_data.json',
                'risk_data.json',
                'metadata.json'
            ];
            
            console.log('Loading data files...');
            
            for (const file of dataFiles) {
                await this.loadDataFile(file);
            }
            
            this.retryCount = 0;
            this.processLoadedData();
            this.broadcastUpdate();
            
        } catch (error) {
            console.error('Error loading data:', error);
            this.handleLoadError(error);
        }
    }
    
    async loadDataFile(filename) {
        try {
            const response = await fetch(`${this.baseUrl}data/${filename}`);
            
            if (!response.ok) {
                throw new Error(`Failed to load ${filename}: ${response.status}`);
            }
            
            const data = await response.json();
            const key = filename.replace('.json', '');
            
            this.dataCache.set(key, data);
            console.log(`✓ Loaded ${filename}`);
            
        } catch (error) {
            console.warn(`Failed to load ${filename}:`, error.message);
            
            // Use fallback data if available
            this.generateFallbackData(filename.replace('.json', ''));
        }
    }
    
    generateFallbackData(dataType) {
        console.log(`Generating fallback data for ${dataType}...`);
        
        switch (dataType) {
            case 'market_data':
                this.dataCache.set('market_data', this.generateFallbackMarketData());
                break;
            case 'network_data':
                this.dataCache.set('network_data', this.generateFallbackNetworkData());
                break;
            case 'agent_data':
                this.dataCache.set('agent_data', this.generateFallbackAgentData());
                break;
            case 'risk_data':
                this.dataCache.set('risk_data', this.generateFallbackRiskData());
                break;
            case 'metadata':
                this.dataCache.set('metadata', {
                    last_updated: new Date().toISOString(),
                    markets_count: 20,
                    data_sources: ['Simulated Data'],
                    update_frequency: 'Real-time simulation',
                    version: '1.0.0'
                });
                break;
        }
    }
    
    generateFallbackMarketData() {
        const markets = [
            'US_SP500', 'US_NASDAQ', 'US_RUSSELL',
            'EU_STOXX', 'DE_DAX', 'FR_CAC40', 'UK_FTSE',
            'JP_NIKKEI', 'AU_ASX', 'CA_TSX',
            'CN_SHANGHAI', 'HK_HANGSENG', 'IN_SENSEX',
            'BR_BOVESPA', 'ZA_JSE', 'KR_KOSPI'
        ];
        
        const data = {};
        markets.forEach(market => {
            data[market] = {
                name: market,
                type: market.startsWith('US_') || market.startsWith('EU_') ? 'developed' : 'emerging',
                current_price: 1000 + Math.random() * 4000,
                last_return: (Math.random() - 0.5) * 0.1,
                volatility: Math.random() * 0.5,
                timestamp: new Date().toISOString()
            };
        });
        
        return data;
    }
    
    generateFallbackNetworkData() {
        const data = {};
        
        for (let scale = 1; scale <= 3; scale++) {
            for (let quantile of [0.05, 0.50, 0.95]) {
                const key = `scale_${scale}_quantile_${quantile.toString().replace('.', '')}`;
                const size = 16;
                
                // Generate random adjacency matrix
                const adjacency = [];
                for (let i = 0; i < size; i++) {
                    const row = [];
                    for (let j = 0; j < size; j++) {
                        row.push(i !== j && Math.random() < 0.3 ? 1 : 0);
                    }
                    adjacency.push(row);
                }
                
                data[key] = {
                    scale: scale,
                    quantile: quantile,
                    adjacency_matrix: adjacency,
                    threshold: 0.1,
                    density: 0.3,
                    nodes: Object.keys(this.dataCache.get('market_data') || {}).slice(0, size),
                    timestamp: new Date().toISOString()
                };
            }
        }
        
        return data;
    }
    
    generateFallbackAgentData() {
        return {
            hft: {
                count: 50,
                avg_performance: 0.9,
                avg_latency: 1.2,
                total_messages: 2500,
                active_ratio: 0.95
            },
            market_maker: {
                count: 20,
                avg_performance: 0.85,
                avg_latency: 2.1,
                total_messages: 1200,
                active_ratio: 0.9
            },
            institutional: {
                count: 30,
                avg_performance: 0.8,
                avg_latency: 3.5,
                total_messages: 600,
                active_ratio: 0.95
            },
            regulator: {
                count: 5,
                avg_performance: 0.98,
                avg_latency: 1.0,
                total_messages: 120,
                active_ratio: 1.0
            },
            timestamp: new Date().toISOString()
        };
    }
    
    generateFallbackRiskData() {
        return {
            components: {
                market: Math.random() * 0.6 + 0.2,
                credit: Math.random() * 0.5 + 0.15,
                liquidity: Math.random() * 0.6 + 0.25,
                operational: Math.random() * 0.4 + 0.1
            },
            indicators: [
                { name: 'VIX', value: 20 + Math.random() * 15, threshold: 25 },
                { name: 'Credit Spreads', value: 120 + Math.random() * 80, threshold: 150 },
                { name: 'Liquidity Ratio', value: 0.6 + Math.random() * 0.4, threshold: 0.8 },
                { name: 'Correlation', value: 0.5 + Math.random() * 0.4, threshold: 0.7 }
            ],
            timestamp: new Date().toISOString()
        };
    }
    
    processLoadedData() {
        // Process market data
        const marketData = this.dataCache.get('market_data');
        if (marketData) {
            this.processMarketData(marketData);
        }
        
        // Process network data
        const networkData = this.dataCache.get('network_data');
        if (networkData) {
            this.processNetworkData(networkData);
        }
        
        // Process agent data
        const agentData = this.dataCache.get('agent_data');
        if (agentData) {
            this.processAgentData(agentData);
        }
        
        // Process risk data
        const riskData = this.dataCache.get('risk_data');
        if (riskData) {
            this.processRiskData(riskData);
        }
        
        // Update metadata
        const metadata = this.dataCache.get('metadata');
        if (metadata) {
            this.lastUpdateTime = new Date(metadata.last_updated);
            this.updateMetadataDisplay(metadata);
        }
    }
    
    processMarketData(data) {
        // Update market displays
        const marketElements = document.querySelectorAll('[data-market]');
        marketElements.forEach(element => {
            const marketName = element.getAttribute('data-market');
            if (data[marketName]) {
                const market = data[marketName];
                this.updateMarketElement(element, market);
            }
        });
    }
    
    updateMarketElement(element, market) {
        const priceElement = element.querySelector('.market-price');
        const changeElement = element.querySelector('.market-change');
        
        if (priceElement) {
            priceElement.textContent = market.current_price.toFixed(2);
        }
        
        if (changeElement) {
            const change = market.last_return * 100;
            changeElement.textContent = `${change > 0 ? '+' : ''}${change.toFixed(2)}%`;
            changeElement.className = `market-change ${change > 0 ? 'positive' : 'negative'}`;
        }
    }
    
    processNetworkData(data) {
        // Update 3D network visualization
        if (window.waveQTEApp && window.waveQTEApp.network3D) {
            window.waveQTEApp.network3D.updateNetworkData(data);
        }
    }
    
    processAgentData(data) {
        // Update agent statistics
        const agentStats = document.querySelectorAll('.agent-stat');
        agentStats.forEach(stat => {
            const agentType = stat.getAttribute('data-agent-type');
            const statType = stat.getAttribute('data-stat-type');
            
            if (data[agentType] && data[agentType][statType]) {
                const value = data[agentType][statType];
                stat.textContent = this.formatStatValue(value, statType);
            }
        });
    }
    
    processRiskData(data) {
        // Update risk gauge
        if (window.waveQTEApp && window.waveQTEApp.riskGauge) {
            window.waveQTEApp.riskGauge.setValue(data.aggregate_risk);
        }
        
        // Update risk components
        Object.keys(data.components).forEach(component => {
            const element = document.querySelector(`[data-risk-component="${component}"]`);
            if (element) {
                const value = data.components[component];
                const valueElement = element.querySelector('.component-value');
                const barElement = element.querySelector('.bar-fill');
                
                if (valueElement) {
                    valueElement.textContent = value.toFixed(2);
                }
                
                if (barElement) {
                    barElement.style.width = `${value * 100}%`;
                }
            }
        });
    }
    
    formatStatValue(value, statType) {
        switch (statType) {
            case 'avg_performance':
                return `${(value * 100).toFixed(1)}%`;
            case 'avg_latency':
                return `${value.toFixed(1)}ms`;
            case 'active_ratio':
                return `${(value * 100).toFixed(0)}%`;
            default:
                return typeof value === 'number' ? value.toLocaleString() : value;
        }
    }
    
    updateMetadataDisplay(metadata) {
        const lastUpdatedElement = document.getElementById('last-updated');
        if (lastUpdatedElement) {
            const updateTime = new Date(metadata.last_updated);
            lastUpdatedElement.textContent = `Last updated: ${updateTime.toLocaleString()}`;
        }
        
        const marketsCountElement = document.getElementById('markets-count');
        if (marketsCountElement) {
            marketsCountElement.textContent = metadata.markets_count;
        }
    }
    
    startPeriodicUpdates() {
        // Check for updates every 5 minutes
        this.updateInterval = setInterval(() => {
            this.loadAllData();
        }, 5 * 60 * 1000);
    }
    
    handleLoadError(error) {
        console.error('Data loading error:', error);
        
        this.retryCount++;
        if (this.retryCount < this.maxRetries) {
            console.log(`Retrying data load (${this.retryCount}/${this.maxRetries})...`);
            setTimeout(() => this.loadAllData(), 5000);
        } else {
            console.log('Max retries reached, using fallback data');
            this.generateAllFallbackData();
        }
    }
    
    generateAllFallbackData() {
        this.generateFallbackData('market_data');
        this.generateFallbackData('network_data');
        this.generateFallbackData('agent_data');
        this.generateFallbackData('risk_data');
        this.generateFallbackData('metadata');
        
        this.processLoadedData();
        this.broadcastUpdate();
    }
    
    broadcastUpdate() {
        const updateEvent = new CustomEvent('realDataUpdate', {
            detail: {
                timestamp: new Date(),
                source: 'real-data-handler',
                data: {
                    market: this.dataCache.get('market_data'),
                    network: this.dataCache.get('network_data'),
                    agents: this.dataCache.get('agent_data'),
                    risk: this.dataCache.get('risk_data'),
                    metadata: this.dataCache.get('metadata')
                }
            }
        });
        
        document.dispatchEvent(updateEvent);
    }
    
    setupEventListeners() {
        // Listen for manual refresh requests
        document.addEventListener('refreshData', () => {
            this.loadAllData();
        });
        
        // Listen for page visibility changes
        document.addEventListener('visibilitychange', () => {
            if (!document.hidden) {
                this.loadAllData();
            }
        });
    }
    
    // Public API
    getData(type) {
        return this.dataCache.get(type);
    }
    
    getLastUpdateTime() {
        return this.lastUpdateTime;
    }
    
    forceRefresh() {
        this.retryCount = 0;
        this.loadAllData();
    }
    
    destroy() {
        if (this.updateInterval) {
            clearInterval(this.updateInterval);
        }
        this.dataCache.clear();
    }
}

// Initialize real data handler
let realDataHandler = null;

document.addEventListener('DOMContentLoaded', () => {
    realDataHandler = new RealDataHandler();
    
    // Make available globally
    window.realDataHandler = realDataHandler;
    
    // Add refresh button functionality
    const refreshButton = document.getElementById('refresh-data');
    if (refreshButton) {
        refreshButton.addEventListener('click', () => {
            realDataHandler.forceRefresh();
        });
    }
});

// Export for use in other modules
window.RealDataHandler = RealDataHandler;