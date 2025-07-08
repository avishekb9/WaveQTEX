// Agent Management System for WaveQTE Network Economics
// Handles agent visualization, monitoring, and control

class AgentNetworkVisualization {
    constructor(container) {
        this.container = container;
        this.svg = null;
        this.simulation = null;
        this.nodes = [];
        this.links = [];
        
        this.init();
    }
    
    init() {
        if (!window.d3) {
            console.warn('D3.js not available, using fallback visualization');
            this.createFallbackVisualization();
            return;
        }
        
        const width = this.container.offsetWidth;
        const height = this.container.offsetHeight;
        
        this.svg = d3.select(this.container)
            .append('svg')
            .attr('width', width)
            .attr('height', height);
        
        this.createAgentNetwork();
    }
    
    createAgentNetwork() {
        // Generate sample agent network data
        this.nodes = [
            { id: 'HFT-1', type: 'hft', x: 50, y: 50 },
            { id: 'HFT-2', type: 'hft', x: 100, y: 50 },
            { id: 'MM-1', type: 'market_maker', x: 150, y: 100 },
            { id: 'MM-2', type: 'market_maker', x: 200, y: 100 },
            { id: 'INST-1', type: 'institutional', x: 100, y: 150 },
            { id: 'INST-2', type: 'institutional', x: 150, y: 150 },
            { id: 'REG-1', type: 'regulator', x: 125, y: 200 }
        ];
        
        this.links = [
            { source: 'HFT-1', target: 'MM-1', strength: 0.8 },
            { source: 'HFT-2', target: 'MM-2', strength: 0.7 },
            { source: 'MM-1', target: 'INST-1', strength: 0.6 },
            { source: 'MM-2', target: 'INST-2', strength: 0.5 },
            { source: 'INST-1', target: 'REG-1', strength: 0.4 },
            { source: 'INST-2', target: 'REG-1', strength: 0.3 }
        ];
        
        this.createD3Network();
    }
    
    createD3Network() {
        const width = this.container.offsetWidth;
        const height = this.container.offsetHeight;
        
        // Create force simulation
        this.simulation = d3.forceSimulation(this.nodes)
            .force('link', d3.forceLink(this.links).id(d => d.id).distance(50))
            .force('charge', d3.forceManyBody().strength(-100))
            .force('center', d3.forceCenter(width / 2, height / 2));
        
        // Create links
        const link = this.svg.append('g')
            .selectAll('line')
            .data(this.links)
            .enter()
            .append('line')
            .attr('stroke', '#1de9b6')
            .attr('stroke-width', d => d.strength * 3)
            .attr('opacity', 0.6);
        
        // Create nodes
        const node = this.svg.append('g')
            .selectAll('circle')
            .data(this.nodes)
            .enter()
            .append('circle')
            .attr('r', 8)
            .attr('fill', d => this.getAgentColor(d.type))
            .attr('stroke', '#ffffff')
            .attr('stroke-width', 2)
            .call(d3.drag()
                .on('start', this.dragstarted.bind(this))
                .on('drag', this.dragged.bind(this))
                .on('end', this.dragended.bind(this)));
        
        // Add labels
        const label = this.svg.append('g')
            .selectAll('text')
            .data(this.nodes)
            .enter()
            .append('text')
            .text(d => d.id)
            .attr('font-size', '10px')
            .attr('fill', '#b8c4d9')
            .attr('text-anchor', 'middle')
            .attr('dy', -15);
        
        // Update positions on simulation tick
        this.simulation.on('tick', () => {
            link
                .attr('x1', d => d.source.x)
                .attr('y1', d => d.source.y)
                .attr('x2', d => d.target.x)
                .attr('y2', d => d.target.y);
            
            node
                .attr('cx', d => d.x)
                .attr('cy', d => d.y);
            
            label
                .attr('x', d => d.x)
                .attr('y', d => d.y);
        });
    }
    
    getAgentColor(type) {
        const colors = {
            hft: '#00e5ff',
            market_maker: '#64ffda',
            institutional: '#1de9b6',
            regulator: '#ffc947'
        };
        return colors[type] || '#ffffff';
    }
    
    dragstarted(event, d) {
        if (!event.active) this.simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
    }
    
    dragged(event, d) {
        d.fx = event.x;
        d.fy = event.y;
    }
    
    dragended(event, d) {
        if (!event.active) this.simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
    }
    
    createFallbackVisualization() {
        // Simple fallback without D3
        const canvas = document.createElement('canvas');
        canvas.width = this.container.offsetWidth;
        canvas.height = this.container.offsetHeight;
        const ctx = canvas.getContext('2d');
        
        // Draw simple agent network
        ctx.fillStyle = '#00e5ff';
        ctx.fillRect(50, 50, 20, 20);
        ctx.fillStyle = '#64ffda';
        ctx.fillRect(150, 100, 20, 20);
        ctx.fillStyle = '#1de9b6';
        ctx.fillRect(100, 150, 20, 20);
        
        // Draw connections
        ctx.strokeStyle = '#1de9b6';
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.moveTo(60, 60);
        ctx.lineTo(160, 110);
        ctx.moveTo(160, 110);
        ctx.lineTo(110, 160);
        ctx.stroke();
        
        this.container.appendChild(canvas);
    }
}

class AgentMonitor {
    constructor() {
        this.agents = [];
        this.metrics = {};
        this.updateInterval = null;
        
        this.init();
    }
    
    init() {
        this.generateAgentData();
        this.startMonitoring();
    }
    
    generateAgentData() {
        this.agents = [
            {
                id: 'HFT-001',
                type: 'high_freq_trader',
                status: 'active',
                performance: 0.987,
                latency: 0.3,
                messages: 1247,
                profit: 12500
            },
            {
                id: 'MM-015',
                type: 'market_maker',
                status: 'active',
                performance: 0.952,
                latency: 0.8,
                messages: 856,
                profit: 8900
            },
            {
                id: 'INST-007',
                type: 'institutional',
                status: 'active',
                performance: 0.934,
                latency: 1.2,
                messages: 634,
                profit: 45000
            },
            {
                id: 'REG-002',
                type: 'regulator',
                status: 'monitoring',
                performance: 0.998,
                latency: 0.5,
                messages: 89,
                profit: 0
            }
        ];
    }
    
    startMonitoring() {
        this.updateInterval = setInterval(() => {
            this.updateAgentMetrics();
            this.updateDisplay();
        }, 2000);
    }
    
    updateAgentMetrics() {
        this.agents.forEach(agent => {
            // Simulate metric updates
            agent.performance += (Math.random() - 0.5) * 0.01;
            agent.performance = Math.max(0.5, Math.min(1, agent.performance));
            
            agent.latency += (Math.random() - 0.5) * 0.1;
            agent.latency = Math.max(0.1, Math.min(5, agent.latency));
            
            agent.messages += Math.floor(Math.random() * 10 - 5);
            agent.messages = Math.max(0, agent.messages);
            
            if (agent.type !== 'regulator') {
                agent.profit += Math.floor(Math.random() * 1000 - 500);
            }
        });
    }
    
    updateDisplay() {
        // Update agent cards
        this.agents.forEach(agent => {
            this.updateAgentCard(agent);
        });
        
        // Update aggregate metrics
        this.updateAggregateMetrics();
    }
    
    updateAgentCard(agent) {
        // Find corresponding agent card and update
        const cards = document.querySelectorAll('.agent-card');
        cards.forEach(card => {
            const cardType = card.classList[1];
            if (this.matchesAgentType(agent.type, cardType)) {
                this.updateCardMetrics(card, agent);
            }
        });
    }
    
    matchesAgentType(agentType, cardType) {
        const typeMap = {
            'high_freq_trader': 'hft',
            'market_maker': 'market-maker',
            'institutional': 'institutional',
            'regulator': 'regulator'
        };
        return typeMap[agentType] === cardType;
    }
    
    updateCardMetrics(card, agent) {
        const metrics = card.querySelectorAll('.metric span:last-child');
        if (metrics.length >= 2) {
            // Update performance metric
            metrics[0].textContent = (agent.performance * 100).toFixed(1) + '%';
            
            // Update latency metric
            metrics[1].textContent = agent.latency.toFixed(1) + 'ms';
        }
    }
    
    updateAggregateMetrics() {
        const totalMessages = this.agents.reduce((sum, agent) => sum + agent.messages, 0);
        const avgPerformance = this.agents.reduce((sum, agent) => sum + agent.performance, 0) / this.agents.length;
        const avgLatency = this.agents.reduce((sum, agent) => sum + agent.latency, 0) / this.agents.length;
        
        this.metrics = {
            totalMessages,
            avgPerformance,
            avgLatency,
            activeAgents: this.agents.filter(a => a.status === 'active').length
        };
    }
    
    getAgent(id) {
        return this.agents.find(agent => agent.id === id);
    }
    
    getAgentsByType(type) {
        return this.agents.filter(agent => agent.type === type);
    }
    
    getMetrics() {
        return this.metrics;
    }
    
    destroy() {
        if (this.updateInterval) {
            clearInterval(this.updateInterval);
        }
    }
}

class MCPProtocolManager {
    constructor() {
        this.protocols = [];
        this.status = {
            messagesPerSecond: 0,
            bandwidth: 0,
            latency: 0,
            successRate: 0
        };
        
        this.init();
    }
    
    init() {
        this.generateProtocolData();
        this.startStatusUpdates();
    }
    
    generateProtocolData() {
        this.protocols = [
            {
                id: 'MCP-001',
                type: 'market_signal',
                participants: ['HFT-001', 'MM-015', 'INST-007'],
                status: 'active',
                throughput: 1247,
                latency: 2.3,
                successRate: 0.992
            },
            {
                id: 'MCP-002',
                type: 'coordination',
                participants: ['MM-015', 'INST-007', 'REG-002'],
                status: 'active',
                throughput: 856,
                latency: 1.8,
                successRate: 0.987
            }
        ];
    }
    
    startStatusUpdates() {
        setInterval(() => {
            this.updateProtocolStatus();
        }, 1000);
    }
    
    updateProtocolStatus() {
        // Update individual protocol metrics
        this.protocols.forEach(protocol => {
            protocol.throughput += Math.floor(Math.random() * 20 - 10);
            protocol.throughput = Math.max(0, protocol.throughput);
            
            protocol.latency += (Math.random() - 0.5) * 0.2;
            protocol.latency = Math.max(0.5, Math.min(10, protocol.latency));
            
            protocol.successRate += (Math.random() - 0.5) * 0.01;
            protocol.successRate = Math.max(0.9, Math.min(1, protocol.successRate));
        });
        
        // Update aggregate status
        this.status.messagesPerSecond = this.protocols.reduce((sum, p) => sum + p.throughput, 0);
        this.status.bandwidth = Math.min(100, (this.status.messagesPerSecond / 2000) * 100);
        this.status.latency = this.protocols.reduce((sum, p) => sum + p.latency, 0) / this.protocols.length;
        this.status.successRate = this.protocols.reduce((sum, p) => sum + p.successRate, 0) / this.protocols.length;
    }
    
    getStatus() {
        return this.status;
    }
    
    getProtocols() {
        return this.protocols;
    }
    
    getProtocol(id) {
        return this.protocols.find(p => p.id === id);
    }
}

// Initialize agent management systems
let agentMonitor = null;
let mcpManager = null;

document.addEventListener('DOMContentLoaded', () => {
    agentMonitor = new AgentMonitor();
    mcpManager = new MCPProtocolManager();
});

// Export for global access
window.AgentNetworkVisualization = AgentNetworkVisualization;
window.AgentMonitor = AgentMonitor;
window.MCPProtocolManager = MCPProtocolManager;