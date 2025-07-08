// Chart Components for WaveQTE Network Economics
// Specialized chart classes and utilities

class NetworkAnimation {
    constructor(canvas, ctx) {
        this.canvas = canvas;
        this.ctx = ctx;
        this.nodes = [];
        this.edges = [];
        this.isPlaying = false;
        this.animationFrame = null;
        this.scale = 'all';
        
        this.generateNetworkData();
    }
    
    generateNetworkData() {
        // Generate sample network data for animation
        const nodeCount = 24;
        
        // Create nodes
        this.nodes = [];
        for (let i = 0; i < nodeCount; i++) {
            this.nodes.push({
                id: i,
                x: Math.random() * this.canvas.width,
                y: Math.random() * this.canvas.height,
                vx: (Math.random() - 0.5) * 2,
                vy: (Math.random() - 0.5) * 2,
                radius: Math.random() * 8 + 4,
                color: i < 12 ? '#00e5ff' : '#64ffda',
                connections: []
            });
        }
        
        // Create edges
        this.edges = [];
        for (let i = 0; i < nodeCount; i++) {
            for (let j = i + 1; j < nodeCount; j++) {
                if (Math.random() < 0.3) {
                    this.edges.push({
                        source: i,
                        target: j,
                        strength: Math.random()
                    });
                    this.nodes[i].connections.push(j);
                    this.nodes[j].connections.push(i);
                }
            }
        }
    }
    
    start() {
        this.isPlaying = true;
        this.animate();
    }
    
    pause() {
        this.isPlaying = false;
        if (this.animationFrame) {
            cancelAnimationFrame(this.animationFrame);
        }
    }
    
    play() {
        this.isPlaying = true;
        this.animate();
    }
    
    animate() {
        if (!this.isPlaying) return;
        
        this.update();
        this.draw();
        
        this.animationFrame = requestAnimationFrame(() => this.animate());
    }
    
    update() {
        // Update node positions
        this.nodes.forEach(node => {
            node.x += node.vx;
            node.y += node.vy;
            
            // Bounce off walls
            if (node.x < node.radius || node.x > this.canvas.width - node.radius) {
                node.vx *= -1;
            }
            if (node.y < node.radius || node.y > this.canvas.height - node.radius) {
                node.vy *= -1;
            }
            
            // Keep in bounds
            node.x = Math.max(node.radius, Math.min(this.canvas.width - node.radius, node.x));
            node.y = Math.max(node.radius, Math.min(this.canvas.height - node.radius, node.y));
        });
    }
    
    draw() {
        // Clear canvas
        this.ctx.fillStyle = 'rgba(10, 11, 15, 0.1)';
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
        
        // Draw edges
        this.ctx.strokeStyle = '#1de9b6';
        this.ctx.lineWidth = 1;
        this.edges.forEach(edge => {
            const source = this.nodes[edge.source];
            const target = this.nodes[edge.target];
            
            this.ctx.globalAlpha = edge.strength * 0.5;
            this.ctx.beginPath();
            this.ctx.moveTo(source.x, source.y);
            this.ctx.lineTo(target.x, target.y);
            this.ctx.stroke();
        });
        
        // Draw nodes
        this.ctx.globalAlpha = 1;
        this.nodes.forEach(node => {
            // Node glow
            const gradient = this.ctx.createRadialGradient(
                node.x, node.y, 0,
                node.x, node.y, node.radius * 2
            );
            gradient.addColorStop(0, node.color);
            gradient.addColorStop(1, 'transparent');
            
            this.ctx.fillStyle = gradient;
            this.ctx.beginPath();
            this.ctx.arc(node.x, node.y, node.radius * 2, 0, Math.PI * 2);
            this.ctx.fill();
            
            // Node core
            this.ctx.fillStyle = node.color;
            this.ctx.beginPath();
            this.ctx.arc(node.x, node.y, node.radius, 0, Math.PI * 2);
            this.ctx.fill();
        });
    }
    
    setScale(scale) {
        this.scale = scale;
        // Adjust animation based on scale
        console.log('Network animation scale changed to:', scale);
    }
    
    updateData() {
        // Simulate data update
        this.nodes.forEach(node => {
            node.radius = Math.random() * 8 + 4;
        });
    }
    
    handleResize() {
        const rect = this.canvas.getBoundingClientRect();
        this.canvas.width = rect.width * window.devicePixelRatio;
        this.canvas.height = rect.height * window.devicePixelRatio;
        this.ctx.scale(window.devicePixelRatio, window.devicePixelRatio);
    }
    
    destroy() {
        this.pause();
        this.nodes = [];
        this.edges = [];
    }
}

class CorrelationHeatmap {
    constructor(container, data) {
        this.container = container;
        this.data = data;
        this.svg = null;
        this.init();
    }
    
    init() {
        if (!window.d3) {
            console.warn('D3.js not available, using fallback visualization');
            this.createFallbackHeatmap();
            return;
        }
        
        const margin = { top: 20, right: 20, bottom: 30, left: 40 };
        const width = this.container.offsetWidth - margin.left - margin.right;
        const height = this.container.offsetHeight - margin.top - margin.bottom;
        
        this.svg = d3.select(this.container)
            .append('svg')
            .attr('width', width + margin.left + margin.right)
            .attr('height', height + margin.top + margin.bottom)
            .append('g')
            .attr('transform', `translate(${margin.left},${margin.top})`);
        
        this.createHeatmap(width, height);
    }
    
    createHeatmap(width, height) {
        const markets = this.data.markets;
        const matrix = this.data.matrix;
        
        const colorScale = d3.scaleSequential(d3.interpolateBlues)
            .domain([0, 1]);
        
        const cellSize = Math.min(width, height) / markets.length;
        
        // Create cells
        const cells = this.svg.selectAll('.cell')
            .data(matrix.flat().map((d, i) => ({
                value: d,
                row: Math.floor(i / markets.length),
                col: i % markets.length
            })))
            .enter()
            .append('rect')
            .attr('class', 'cell')
            .attr('x', d => d.col * cellSize)
            .attr('y', d => d.row * cellSize)
            .attr('width', cellSize)
            .attr('height', cellSize)
            .attr('fill', d => colorScale(d.value))
            .attr('stroke', '#1a1d29')
            .attr('stroke-width', 1);
        
        // Add labels
        this.svg.selectAll('.label-x')
            .data(markets)
            .enter()
            .append('text')
            .attr('class', 'label-x')
            .attr('x', (d, i) => (i + 0.5) * cellSize)
            .attr('y', height + 15)
            .attr('text-anchor', 'middle')
            .attr('font-size', '10px')
            .attr('fill', '#b8c4d9')
            .text(d => d.substring(0, 3));
    }
    
    createFallbackHeatmap() {
        // Simple fallback without D3
        const canvas = document.createElement('canvas');
        canvas.width = this.container.offsetWidth;
        canvas.height = this.container.offsetHeight;
        const ctx = canvas.getContext('2d');
        
        const cellSize = Math.min(canvas.width, canvas.height) / this.data.markets.length;
        
        this.data.matrix.forEach((row, i) => {
            row.forEach((value, j) => {
                const intensity = Math.floor(value * 255);
                ctx.fillStyle = `rgb(0, ${intensity}, ${255 - intensity})`;
                ctx.fillRect(j * cellSize, i * cellSize, cellSize, cellSize);
            });
        });
        
        this.container.appendChild(canvas);
    }
}

class RiskGauge {
    constructor(canvas) {
        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');
        this.value = 0;
        this.targetValue = 0;
        this.animationFrame = null;
        
        this.canvas.width = 200;
        this.canvas.height = 200;
        
        this.animate();
    }
    
    setValue(value) {
        this.targetValue = Math.max(0, Math.min(1, value));
    }
    
    animate() {
        // Smooth animation to target value
        this.value += (this.targetValue - this.value) * 0.1;
        
        this.draw();
        
        this.animationFrame = requestAnimationFrame(() => this.animate());
    }
    
    draw() {
        const centerX = this.canvas.width / 2;
        const centerY = this.canvas.height / 2;
        const radius = 80;
        
        this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
        
        // Background arc
        this.ctx.beginPath();
        this.ctx.arc(centerX, centerY, radius, Math.PI, 0);
        this.ctx.lineWidth = 20;
        this.ctx.strokeStyle = '#2a2f47';
        this.ctx.stroke();
        
        // Value arc
        const angle = Math.PI * this.value;
        this.ctx.beginPath();
        this.ctx.arc(centerX, centerY, radius, Math.PI, Math.PI + angle);
        this.ctx.lineWidth = 20;
        
        // Color based on risk level
        if (this.value < 0.3) {
            this.ctx.strokeStyle = '#4caf50';
        } else if (this.value < 0.7) {
            this.ctx.strokeStyle = '#ffc947';
        } else {
            this.ctx.strokeStyle = '#ff5722';
        }
        
        this.ctx.stroke();
        
        // Value text
        this.ctx.fillStyle = '#ffffff';
        this.ctx.font = '24px monospace';
        this.ctx.textAlign = 'center';
        this.ctx.fillText(
            (this.value * 100).toFixed(1) + '%',
            centerX,
            centerY + 10
        );
    }
    
    destroy() {
        if (this.animationFrame) {
            cancelAnimationFrame(this.animationFrame);
        }
    }
}

// Export classes for use in main application
window.NetworkAnimation = NetworkAnimation;
window.CorrelationHeatmap = CorrelationHeatmap;
window.RiskGauge = RiskGauge;