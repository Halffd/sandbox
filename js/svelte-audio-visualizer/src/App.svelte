<script>
  import { onMount } from 'svelte';

  // Reactive state variables using runes
  let audioContext = $state(null);
  let analyser = $state(null);
  let dataArray = $state(null);
  let source = $state(null);
  let isRecording = $state(false);
  let canvas = $state(null);

  // Canvas dimensions
  const WIDTH = 800;
  const HEIGHT = 400;

  // Set up audio context and analyser
  async function setupAudio() {
    try {
      const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
      audioContext = new AudioContext();
      analyser = audioContext.createAnalyser();
      source = audioContext.createMediaStreamSource(stream);
      source.connect(analyser);
      analyser.fftSize = 2048;
      dataArray = new Uint8Array(analyser.frequencyBinCount);
    } catch (err) {
      console.error('Error accessing microphone:', err);
    }
  }

  // Draw the audio wave on the canvas
  function draw() {
    if (!isRecording) return;
    requestAnimationFrame(draw);
    analyser.getByteTimeDomainData(dataArray);
    const ctx = canvas.getContext('2d');
    ctx.clearRect(0, 0, WIDTH, HEIGHT);
    ctx.lineWidth = 2;
    ctx.strokeStyle = 'rgb(0, 0, 0)';
    ctx.beginPath();
    const sliceWidth = WIDTH / dataArray.length;
    let x = 0;
    for (let i = 0; i < dataArray.length; i++) {
      const v = dataArray[i] / 128.0;
      const y = (v * HEIGHT) / 2;
      if (i === 0) {
        ctx.moveTo(x, y);
      } else {
        ctx.lineTo(x, y);
      }
      x += sliceWidth;
    }
    ctx.lineTo(WIDTH, HEIGHT / 2);
    ctx.stroke();
  }

  // Start recording and visualization
  function startRecording() {
    if (!audioContext) {
      setupAudio().then(() => {
        isRecording = true;
        draw();
      });
    } else {
      isRecording = true;
      draw();
    }
  }

  // Stop recording and visualization
  function stopRecording() {
    isRecording = false;
  }

  // Clean up when the component is destroyed
  onMount(() => {
    return () => {
      if (audioContext) {
        audioContext.close();
      }
    };
  });
</script>

<!-- Canvas for rendering the audio wave -->
<canvas bind:this={canvas} width={WIDTH} height={HEIGHT}></canvas>

<!-- Buttons to control recording -->
<button on:click={startRecording} disabled={isRecording}>Start Recording</button>
<button on:click={stopRecording} disabled={!isRecording}>Stop Recording</button>